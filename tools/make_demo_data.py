#!/usr/bin/env python3
"""
Synthetic SLDP data in the exact shape the Kobo API returns, for testing the
dashboard pipeline without real patients.

Writes registration_latest.json, cem_latest.json and adr_latest.json
(the same snapshot files R/pipeline.R saves after a live pull) into the folder
given with --out. The form definition (survey and choices) is read from the
three XLSForm files, so the JSON keys carry the real group paths
("site/patient_id", "ae_event/ae_suspected/ae_p1/ae_p1_which").

Every name, phone number and ID here is invented.

  python3 tools/make_demo_data.py --forms <folder with the 3 xlsx> --out <raw dir> [--today 2026-11-15] [--n 650]
Then:  SLDP_USE_CACHE=true SLDP_RAW_DIR=<raw dir> SLDP_TODAY=<today> Rscript R/build_dashboard_data.R
"""
import argparse, json, os, random, uuid, glob
from datetime import date, datetime, timedelta
import openpyxl

ap = argparse.ArgumentParser()
ap.add_argument("--forms", required=True)
ap.add_argument("--out", required=True)
ap.add_argument("--today", default="2026-11-15")
ap.add_argument("--start", default="2026-10-01")
ap.add_argument("--n", type=int, default=650)
ap.add_argument("--seed", type=int, default=20261115)
a = ap.parse_args()
R = random.Random(a.seed)
TODAY = date.fromisoformat(a.today)
START = date.fromisoformat(a.start)

# ---------------------------------------------------------------- form definitions and key paths
def load_form(pattern):
    f = glob.glob(os.path.join(a.forms, pattern))[0]
    wb = openpyxl.load_workbook(f)
    ws = wb["survey"]; hdr = [c.value for c in ws[1]]
    survey, paths, stack = [], {}, []
    for row in ws.iter_rows(min_row=2, values_only=True):
        r = dict(zip(hdr, row))
        t, n = (str(r.get("type") or "").strip()), r.get("name")
        if not t:
            continue
        if t in ("end_group", "end group", "end_repeat", "end repeat"):
            stack.pop(); continue
        base = t.split()[0]
        item = {"type": base, "name": n, "$autoname": n}
        if base in ("select_one", "select_multiple"):
            item["select_from_list_name"] = t.split()[1]
        if r.get("label"):
            item["label"] = [r["label"]]
        survey.append(item)
        if base in ("begin_group", "begin_repeat"):
            stack.append(n); continue
        if n:
            paths[n] = "/".join(stack + [n])
    ch = wb["choices"]; ch_h = [c.value for c in ch[1]]
    choices = []
    for row in ch.iter_rows(min_row=2, values_only=True):
        c = dict(zip(ch_h, row))
        if not c.get("list_name"):
            continue
        d = {"list_name": c["list_name"], "name": str(c["name"]), "label": [c.get("label") or str(c["name"])]}
        for k in ("district", "cem"):
            if c.get(k):
                d[k] = c[k]
        choices.append(d)
    return {"survey": survey, "choices": choices}, paths

F1, P1 = load_form("SLDP_Form1*.xlsx")
F2, P2 = load_form("SLDP_Form2*.xlsx")
F3, P3 = load_form("SLDP_Form3*.xlsx")

def put(rec, paths, **kv):
    for k, v in kv.items():
        if v is None or v == "":
            continue
        rec[paths[k]] = v if isinstance(v, (list, dict)) else str(v)

def meta(rec, when, version):
    rec["_id"] = R.randint(10**8, 10**9)
    rec["_uuid"] = str(uuid.UUID(int=R.getrandbits(128)))
    rec["_submission_time"] = (when + timedelta(minutes=R.randint(5, 240))).strftime("%Y-%m-%dT%H:%M:%S")
    rec["__version__"] = version
    rec["start_time"] = when.strftime("%Y-%m-%dT%H:%M:%S.000+00:00")
    rec["end_time"] = (when + timedelta(minutes=R.randint(6, 25))).strftime("%Y-%m-%dT%H:%M:%S.000+00:00")
    return rec

def at(d, h0=8, h1=16):
    return datetime(d.year, d.month, d.day, R.randint(h0, h1), R.randint(0, 59))

# ---------------------------------------------------------------- reference lists
fac = [c for c in F1["choices"] if c["list_name"] == "facility"]
cem_fac = [c for c in fac if c.get("cem") == "yes"]
non_cem = [c for c in fac if c.get("cem") != "yes"]
TARGET = {"Abokobi": 93, "Adabraka": 163, "Ashaiman": 78, "Civil Service": 48, "Community 22": 41, "Dome": 583,
          "Ga East": 134, "Regional": 72, "La Polyclinic": 78, "Legon": 210}
def weight_of(c):
    for k, v in TARGET.items():
        if k.lower() in c["label"][0].lower():
            return v
    return 60
# Each site has its own pace (some ahead of target, some behind)
pace = {c["name"]: R.uniform(0.35, 1.25) for c in cem_fac}

FIRST = ["Ama", "Kofi", "Akosua", "Kwame", "Abena", "Yaw", "Esi", "Kojo", "Adwoa", "Kwabena", "Efua", "Kwaku", "Afia", "Nii", "Naa",
         "Dede", "Korkor", "Tetteh", "Selorm", "Edem", "Mawuli", "Dzifa", "Ekow", "Araba", "Fiifi", "Mansa", "Zainab", "Ibrahim"]
LAST = ["Mensah", "Owusu", "Boateng", "Asante", "Addo", "Quaye", "Lamptey", "Tetteh", "Ofori", "Agyeman", "Nartey", "Amoah",
        "Darko", "Annan", "Ankrah", "Bortey", "Sackey", "Kumi", "Adjei", "Danso", "Yeboah", "Abubakar"]
def phone():
    return "0" + R.choice("25") + str(R.randint(10000000, 99999999))
def pid():
    return "GA-" + "".join(R.choice("0123456789ABCDEF") for _ in range(8))
def hexid(n=6):
    return "".join(R.choice("0123456789abcdef") for _ in range(n))

def rec_dose(w):
    return "eighth" if w < 10 else "quarter" if w < 15 else "half" if w < 30 else "one"
def act_dose(w):   # ACT weight bands (AL): used by the alternative dosing table
    return "eighth" if w < 15 else "quarter" if w < 25 else "half" if w < 35 else "one"
MG = {"eighth": 1.875, "quarter": 3.75, "half": 7.5, "one": 15}

reg_recs, cem_recs, adr_recs = [], [], []
patients = []

# ---------------------------------------------------------------- FORM 1: registrations
days = (TODAY - START).days + 1
for i in range(a.n):
    # Enrolment grows over the first weeks of the pilot
    off = int((R.random() ** 0.75) * days)
    rdate = START + timedelta(days=min(off, days - 1))
    if R.random() < 0.8:
        f = R.choices(cem_fac, weights=[weight_of(c) * pace[c["name"]] for c in cem_fac])[0]
    else:
        f = R.choice(non_cem)
    cem_site = f.get("cem") == "yes"
    sex = R.choice(["male", "female"])
    yrs = R.choices([R.uniform(0.6, 4.99), R.uniform(5, 14.9), R.uniform(15, 70)], weights=[18, 27, 55])[0]
    months = int(yrs * 12)
    w = round(max(6.5, (8 + 2.2 * yrs) if yrs < 13 else R.gauss(62, 11)), 1)
    p = {"pid": pid(), "fac": f["name"], "district": f.get("district"), "cem_site": cem_site, "rdate": rdate, "sex": sex,
         "months": months, "years": yrs, "weight": w, "first": R.choice(FIRST), "last": R.choice(LAST), "phone": phone()}
    rec = {}
    test_done = "yes" if R.random() < 0.96 else "no"
    ttype = R.choices(["rdt", "microscopy", "both"], weights=[70, 18, 12])[0]
    pf = test_done == "yes" and R.random() < 0.9
    put(rec, P1, district=f.get("district"), facility=f["name"], facility_cem="yes" if cem_site else "no",
        registration_date=rdate.isoformat(), patient_uuid=hexid(32), patient_id=p["pid"], first_name=p["first"], last_name=p["last"],
        phone=p["phone"], age_unit="years" if months >= 24 else "months", age_value=int(yrs) if months >= 24 else months,
        age_months=months, age_years=round(yrs, 2), sex=sex, nhis_status=R.choice(["valid_nhis", "valid_nhis", "not_insured", "invalid_nhis"]),
        weight=w, test_done=test_done)
    if test_done == "yes":
        put(rec, P1, test_type=ttype, pf_positive="yes" if pf else "no", severity="uncomplicated")
        if ttype in ("rdt", "both"):
            put(rec, P1, rdt_result="pf_pos" if pf else R.choice(["negative", "other_pos"]))
        if ttype in ("microscopy", "both"):
            put(rec, P1, micro_result="pf_seen" if pf else "none_seen", micro_species="pf" if pf else "none",
                parasite_density=R.randint(200, 90000) if pf else None)
    else:
        put(rec, P1, pf_positive="no")
    flags = {}
    for k, pr in [("pq_allergy", 0.004), ("severe_anaemia", 0.02), ("dark_urine", 0.015), ("jaundice", 0.01)]:
        flags[k] = "yes" if R.random() < pr else "no"
    preg = bf = "no"
    if sex == "female" and 15 <= yrs < 50:
        preg = "yes" if R.random() < 0.05 else ("not_sure" if R.random() < 0.02 else "no")
        bf = "yes" if R.random() < 0.06 else "no"
    eligible = pf and months >= 12 and preg == "no" and bf == "no" and all(v == "no" for v in flags.values())
    if pf:
        put(rec, P1, pregnant=preg if sex == "female" and yrs >= 10 else None, breastfeeding_u1=bf if sex == "female" and yrs >= 10 else None, **flags)
    put(rec, P1, eligibility="eligible" if eligible else "not_eligible", hx_haemolysis="yes" if R.random() < 0.02 else "no")
    act = R.random() < 0.97
    act_type = R.choices(["al", "asaq", "ap", "dhap"], weights=[78, 12, 4, 6])[0]
    put(rec, P1, act_dispensed="yes" if act else "no")
    if act:
        put(rec, P1, act_type=act_type, act_start_date=rdate.isoformat())
    else:
        put(rec, P1, act_not_dispensed_reason=R.choice(["stockout", "referred", "refused"]))
    given = False
    if eligible and act:
        clin_dis = R.random() < 0.02
        put(rec, P1, clinician_eligible="no" if clin_dis else "yes", clinician_disagrees="yes" if clin_dis else "no")
        accepted = R.random() < 0.93
        put(rec, P1, pq_accepted="yes" if accepted else "no")
        if not accepted:
            put(rec, P1, pq_not_given_reasons=" ".join(R.sample(["side_effects", "blood", "why_another", "not_needed", "g6pd_unknown",
                                                                "bad_experience", "consult", "no_reason"], R.choice([1, 1, 2]))))
        else:
            given = R.random() < 0.95
            put(rec, P1, pq_given="yes" if given else "no")
            if not given:
                put(rec, P1, pq_nd_reason=R.choice(["stockout", "stockout", "clinician", "other"]))
    p["given"] = given
    if given:
        rd, ad = rec_dose(w), act_dose(w)
        u = R.random()
        dose = rd if u < 0.88 else ad if u < 0.96 else R.choice(["eighth", "quarter", "half", "one"])
        flag = "yes" if dose not in (rd, ad) else "no"
        basis = "both" if dose == rd == ad else "sldp_table" if dose == rd else "act_table" if dose == ad else "neither"
        pqdt = at(rdate)
        p["pq_date"] = rdate
        put(rec, P1, pq_brand=R.choice(["generic", "generic", "other"]), rec_dose=rd, act_rec_dose=ad, pq_strength="s15", pq_dose=dose,
            pq_dose_mg=MG[dose], mg_per_kg=round(MG[dose] / w, 3), dose_flag=flag, dose_basis=basis, dose_confirm="yes" if flag == "yes" else None,
            dot="yes" if R.random() < 0.9 else "no", with_food_ans="yes", vomit30="no",
            pq_datetime=pqdt.strftime("%Y-%m-%dT%H:%M:00.000+00:00"), pq_date=rdate.isoformat(), counselled="yes",
            other_meds_any="yes" if R.random() < 0.25 else "no")
    enrolled = given and cem_site and R.random() < 0.9
    p["enrolled"] = enrolled
    if given and cem_site:
        put(rec, P1, cem_enrolled="yes" if enrolled else "no")
    if enrolled:
        put(rec, P1, phone_final=p["phone"], alt_contact_name=R.choice(FIRST) + " " + R.choice(LAST), alt_contact_phone=phone(), cem_card_done="yes")
    put(rec, P1, adr_today="no", entry_name="Officer " + R.choice(LAST))
    patients.append(p)
    reg_recs.append(meta(rec, at(rdate), "v2026111401"))

# ---------------------------------------------------------------- FORM 2: CEM contacts and adverse events
SCHED = [("d0", 0), ("d3", 3), ("d7", 7), ("d14", 14), ("d28", 28)]
SIGNS = ["nausea", "vomiting", "abdominal_pain", "headache", "dizziness", "malaise", "rash", "itching", "dark_urine", "jaundice",
         "pallor", "weakness", "fast_heartbeat"]
SW = [14, 10, 9, 12, 10, 8, 4, 4, 3, 2, 3, 6, 2]

def ae_item(p, vdate, day, kind="new", prev=None, hb_drop=False, death=False):
    it = {}
    def s(**kv):
        put(it, P2, **kv)
    if kind == "new":
        eid = f"{p['pid']}-E{hexid(6).upper()}"
        signs = sorted(set(R.choices(SIGNS, weights=SW, k=R.choice([1, 1, 2, 3]))))
        if hb_drop:
            signs = sorted(set(signs + ["hb_drop"] + (["pallor"] if R.random() < .5 else [])))
        if death:
            signs = sorted(set(signs + ["dark_urine", "jaundice"]))
        onset = vdate - timedelta(days=R.randint(0, min(2, max(day, 0))))
        sev = death or hb_drop and R.random() < 0.35 or R.random() < 0.025
        inten = "severe" if sev else R.choices(["mild", "moderate", "severe"], weights=[66, 28, 6])[0]
        crit = ["death"] if death else (R.sample(["hospitalisation", "life_threatening", "other_important"], 1) if sev else [])
        outc = "D" if death else R.choices(["R1", "R2", "N", "U"], weights=[50, 35, 12, 3])[0]
        mgmt = ["admitted", "hb_checked"] + (["transfused"] if R.random() < .4 else []) if sev else R.sample(["reassured", "symptomatic", "fluids", "hb_checked", "none"], 1)
        late = R.random() < (0.25 if sev else 0.05)
        focal = onset + timedelta(days=R.randint(8, 12) if late and sev else R.randint(0, 3))
        s(ae_n=1, ae_type="new", ae_uuid=hexid(32), ae_new_id=eid, ae_type_saved="new", ae_event_id=eid, ae_signs=" ".join(signs),
          ae_desc="Synthetic event", ae_onset=onset.isoformat(), ae_day=(onset - p["pq_date"]).days, ae_stopped="yes" if outc in ("R1",) else "no",
          ae_mgmt=" ".join(mgmt), ae_intensity=inten, ae_outcome=outc, ae_serious="yes" if sev else "no", ae_criteria=" ".join(crit) or None,
          ae_focal_informed="yes", ae_focal_date=min(focal, TODAY).isoformat(), ae_cm_any="yes" if R.random() < .3 else "no")
        nprod = R.choice([1, 1, 2])
        prods = ["sldp", "act"][:nprod] if R.random() < .85 else ["act"]
        s(ae_p_n=len(prods))
        for k, w in enumerate(prods, 1):
            s(**{f"ae_p{k}_which": w, f"ae_p{k}_generic": "Primaquine phosphate" if w == "sldp" else "Artemether-lumefantrine"})
        p.setdefault("events", []).append({"id": eid, "onset": onset, "serious": sev, "outcome": outc})
        return it, eid
    ev = prev
    outc = R.choices(["R1", "R2", "S", "N"], weights=[70, 20, 3, 7])[0]
    s(ae_n=1, ae_type="update", ae_prev_id=ev["id"], ae_uuid=hexid(32), ae_type_saved="update", ae_event_id=ev["id"],
      ae_outcome=outc, ae_stopped="yes" if outc == "R1" else "no", ae_serious="yes" if ev["serious"] else "no",
      ae_intensity=R.choice(["mild", "moderate"]), ae_stop_date=vdate.isoformat() if outc == "R1" else None)
    ev["outcome"] = outc
    return it, ev["id"]

for p in patients:
    if not p["enrolled"]:
        continue
    d0 = p["pq_date"]
    g6 = R.choices(["unknown", "normal", "deficient"], weights=[80, 17, 3])[0]
    base = R.gauss(12.4 if p["years"] >= 15 else 11.3, 1.5)
    if p["years"] < 5:
        base -= 0.6
    p["hb0"] = round(max(6.2, base), 1)
    drop_mu = -1.9 if g6 == "deficient" else -0.45
    hb3 = round(p["hb0"] + R.gauss(drop_mu, 0.75), 1)
    consent = "no" if R.random() < 0.03 else "yes"
    # Engagement: some participants stop answering partway (lost to follow-up), a few withdraw, one or two die
    drop_after = R.choices([99, 3, 7, 14], weights=[86, 3, 6, 5])[0]
    withdraw_at = "d14" if R.random() < 0.015 else None
    died = R.random() < 0.0025
    late = R.random() < 0.15
    for v, offd in SCHED:
        due = d0 + timedelta(days=offd)
        vdate = due + timedelta(days=R.choice([0, 0, 0, 1, 1, 2]) + (R.randint(2, 5) if late and v != "d0" else 0))
        if vdate > TODAY:
            break
        if v != "d0" and consent == "no":
            break
        if v != "d0" and R.random() < 0.06 and offd > drop_after - 3:  # simply never contacted (overdue in the queue)
            break
        phone_v = v in ("d7", "d14", "d28")
        attempts = []
        if phone_v:
            failing = offd > drop_after
            n_fail = (R.choice([1, 2]) if failing else (1 if R.random() < 0.18 else 0))
            for k in range(n_fail):
                attempts.append(("fail", vdate + timedelta(days=k)))
            if not failing:
                attempts.append(("ok", vdate + timedelta(days=n_fail)))
        elif v == "d3" and offd > drop_after:
            break
        else:
            attempts.append(("ok", vdate))
        for k, (res, adate) in enumerate(attempts):
            if adate > TODAY:
                break
            rec = {}
            hb_alert = False
            put(rec, P2, patient_id=p["pid"], reg_found="yes", r_first_name=p["first"], r_cem_enrolled="yes", r_pq_date=d0.isoformat(),
                day0_date=d0.isoformat(), facility=p["fac"], district=p["district"], sex=p["sex"], age_months=p["months"],
                visit_uuid=hexid(32), visit_date=adate.isoformat(), visit=v, interviewer="Focal " + R.choice(LAST),
                mode="phone" if phone_v else "facility", days_since_day0=(adate - d0).days,
                timing_off="yes" if (adate - due).days > dict(d0=1, d3=2, d7=3, d14=3, d28=5)[v] else "no")
            items = []
            if v == "d0":
                put(rec, P2, consent=consent)
                if consent == "yes":
                    lab = 0 if p["hb0"] >= (13 if p["sex"] == "male" and p["years"] >= 15 else 11.5) else 1 if p["hb0"] >= 10 else 2 if p["hb0"] >= 8 else 3
                    put(rec, P2, g6pd_status=g6, diabetes="no", hypertension="yes" if p["years"] > 40 and R.random() < .25 else "no",
                        f_temp=round(R.gauss(37.6, .8), 1), f_pulse=R.randint(70, 120), f_hb=p["hb0"], f_lab_grade=lab, f_clinical_grade="none" if lab < 2 else "mild",
                        f_status="continuing")
                put(rec, P2, completed="yes" if consent == "yes" else "no")
            elif v == "d3":
                hb_alert = hb3 - p["hb0"] <= -2
                lab = 0 if hb3 >= 11.5 else 1 if hb3 >= 10 else 2 if hb3 >= 8 else 3
                du = "yes" if (hb_alert and R.random() < .4) or R.random() < .015 else "no"
                put(rec, P2, g_full_act="yes" if R.random() < .93 else "no", g_dark_urine=du, g_pallor="yes" if hb_alert and R.random() < .5 else "no",
                    g_fatigue="yes" if R.random() < .12 else "no", g_jaundice="yes" if R.random() < .01 else "no", g_hospitalised="no",
                    g_new_meds="no", g_hb=hb3, g_lab_grade=lab, d0_hb=p["hb0"], hb_change=round(hb3 - p["hb0"], 1), hb_alert="yes" if hb_alert else "no",
                    g_repeat_test="yes" if R.random() < .3 else "no", g_status="continuing", completed="yes")
                p["hb3"] = hb3
            else:
                if res == "fail":
                    put(rec, P2, h_success="no", h_attempt=min(k + 1, 4), h_fail_reason=R.choices(["no_answer", "wrong_number", "declined"], weights=[80, 12, 8])[0],
                        h_next_attempt=(adate + timedelta(days=1)).isoformat(), completed="no")
                else:
                    wd = withdraw_at == v
                    put(rec, P2, h_success="yes", h_withdrawing="yes" if wd else "no", completed="yes", h_status="died" if died and v == "d7" else ("continuing" if not wd else None))
                    for sname in ["dark_urine", "pallor", "jaundice", "fatigue", "dizziness", "fever", "nausea", "vomiting", "rash"]:
                        put(rec, P2, **{"h_" + sname: "yes" if R.random() < .03 else "no"})
                    put(rec, P2, h_hospitalised="no", h_new_meds="yes" if R.random() < .05 else "no", h_ae="no")
                    if v == "d28":
                        put(rec, P2, h_d28_outcome="well", m_status="completed", m_date=adate.isoformat())
                    if wd:
                        put(rec, P2, m_status="withdrawn", m_date=adate.isoformat())
            # Adverse events
            ae_new = (v == "d0" and consent == "yes" and R.random() < .04) or (v == "d3" and (hb_alert or R.random() < .1)) or \
                     (v in ("d7", "d14") and res == "ok" and R.random() < .04)
            is_death = died and v == "d7" and res == "ok"
            if ae_new or is_death:
                it, _ = ae_item(p, adate, (adate - d0).days, hb_drop=hb_alert, death=is_death)
                items.append(it)
                if R.random() < .12 and not is_death:   # a second event the same day
                    it2, _ = ae_item(p, adate, (adate - d0).days)
                    items.append(it2)
            elif res == "ok" and v not in ("d0",) and p.get("events"):
                open_ev = [e for e in p["events"] if e["outcome"] not in ("R1", "D", "S") and e["onset"] < adate]
                for e in open_ev:
                    if R.random() < .8:
                        it, _ = ae_item(p, adate, (adate - d0).days, kind="update", prev=e)
                        items.append(it)
            if items:
                if v in ("d7", "d14", "d28") and res == "ok":
                    rec[P2["h_ae"]] = "yes"
                rec["ae_event"] = items
                put(rec, P2, ae_flag="yes" if hb_alert or is_death else "no", ae_any="yes")
            elif res == "ok" and v in ("d0", "d3") and consent == "yes":
                put(rec, P2, ae_flag="no", ae_any="no")
            status = "died" if is_death else ("withdrew" if withdraw_at == v and res == "ok" else ("continuing" if res == "ok" else None))
            put(rec, P2, status_now=status)
            if is_death:
                put(rec, P2, m_status="died", m_date=adate.isoformat())
            cem_recs.append(meta(rec, at(adate), "v2026111401"))
        if died and v == "d7" or (withdraw_at == v):
            break

# ---------------------------------------------------------------- FORM 3: spontaneous reports (not enrolled in CEM)
for p in patients:
    if not p["given"] or p["enrolled"]:
        continue
    if R.random() > 0.12:
        continue
    onset = p["pq_date"] + timedelta(days=R.randint(0, 6))
    if onset > TODAY:
        continue
    sev = R.random() < 0.08
    lag = R.randint(0, 3) if R.random() < .85 else R.randint(8, 40)
    rdate = min(onset + timedelta(days=lag), TODAY)
    eid = f"{p['pid']}-E{hexid(6).upper()}"
    signs = sorted(set(R.choices(SIGNS, weights=SW, k=R.choice([1, 2]))))
    rec = {}
    put(rec, P3, patient_id=p["pid"], reg_found="yes", r_first_name=p["first"], r_cem_enrolled="no", report_type="new", report_uuid=hexid(32),
        new_event_id=eid, event_id=eid, facility=p["fac"], district=p["district"], sex=p["sex"], age_years=round(p["years"], 1),
        day0_date=p["pq_date"].isoformat(), reported_through="cem_site_not_enrolled" if p["cem_site"] else "non_cem", cem_block="no",
        a_pregnancy="not_pregnant" if p["sex"] == "female" else None, b_signs=" ".join(signs), b_onset=onset.isoformat(),
        b_day=(onset - p["pq_date"]).days, b_stopped="yes", b_mgmt=R.choice(["reassured", "symptomatic", "hb_checked"]),
        b_intensity="severe" if sev else R.choice(["mild", "mild", "moderate"]), c_outcome=R.choice(["R1", "R2", "N"]),
        s_serious="yes" if sev else "no", s_criteria="hospitalisation" if sev else None, e_any="no",
        f_name=R.choice(FIRST) + " " + R.choice(LAST), f_profession=R.choice(["Pharmacist", "Nurse", "Medicine counter assistant", "Physician assistant"]),
        f_report_date=rdate.isoformat(), report_lag=(rdate - onset).days)
    prods = [("sldp", "Primaquine phosphate")] + ([("act", "Artemether-lumefantrine")] if R.random() < .4 else [])
    rep = P3["p_which"].rsplit("/", 1)[0]
    rec[rep] = [{P3["p_which"]: w, P3["p_generic"]: g} for w, g in prods]
    adr_recs.append(meta(rec, at(rdate), "v2026111401"))
    if R.random() < 0.35 and rdate + timedelta(days=7) <= TODAY:
        upd = {}
        ud = rdate + timedelta(days=R.randint(5, 10))
        put(upd, P3, patient_id=p["pid"], reg_found="yes", r_cem_enrolled="no", report_type="update", existing_event_id=eid, event_id=eid,
            report_uuid=hexid(32), facility=p["fac"], district=p["district"], sex=p["sex"], reported_through="non_cem", cem_block="no",
            c_outcome="R1", s_serious="yes" if sev else "no", f_name="Reporter " + R.choice(LAST), f_report_date=ud.isoformat())
        adr_recs.append(meta(upd, at(ud), "v2026111401"))

# ---------------------------------------------------------------- training records (first name TEST): must be excluded
for k in range(3):
    rec = {}
    tid = pid()
    put(rec, P1, facility="GA0403", facility_cem="yes", registration_date=TODAY.isoformat(), patient_id=tid, first_name="TEST", last_name=f"Case{k}",
        phone=phone(), age_months=300, sex="male", weight=60, test_done="yes", pf_positive="yes", eligibility="eligible", act_dispensed="yes",
        pq_accepted="yes", pq_given="yes", pq_dose="one", pq_date=TODAY.isoformat(), cem_enrolled="yes", phone_final=phone())
    reg_recs.append(meta(rec, at(TODAY), "v2026111401"))
    c = {}
    put(c, P2, patient_id=tid, facility="GA0403", visit="d0", visit_date=TODAY.isoformat(), consent="yes", interviewer="TEST officer", completed="yes", f_hb=4.0)
    cem_recs.append(meta(c, at(TODAY), "v2026111401"))

os.makedirs(a.out, exist_ok=True)
pulled = datetime.combine(TODAY, datetime.min.time()).replace(hour=12).strftime("%Y-%m-%d %H:%M:%S GMT")
for name, form, recs, title in [("registration", F1, reg_recs, "SLDP Form 1: Patient Register"),
                                ("cem", F2, cem_recs, "SLDP Form 2: CEM Form (Days 0, 3, 7, 14, 28)"),
                                ("adr", F3, adr_recs, "SLDP Form 3: ADR/SAE Report (Patients Not Enrolled in CEM)")]:
    snap = {"pulled_at": pulled, "form": {"name": title, "version": "v2026111401", **form}, "records": recs}
    with open(os.path.join(a.out, f"{name}_latest.json"), "w") as fh:
        json.dump(snap, fh)
    print(name, len(recs), "records")
print("patients", len(patients), "enrolled", sum(p["enrolled"] for p in patients))
