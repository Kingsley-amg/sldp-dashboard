// Encrypts the private follow-up queue for the password-locked dashboard page.
//
//   QUEUE_PASSWORD=... node tools/encrypt_queue.mjs <queue.json> docs/data/queue.enc.json
//
// The queue (names, phone numbers) is produced by R/build_dashboard_data.R outside the
// published folder. Only the encrypted file is published. The page decrypts it in the
// browser with the same password (Web Crypto): PBKDF2-SHA256 key derivation, AES-256-GCM.
// Without QUEUE_PASSWORD no queue is published, only a placeholder saying so.
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { dirname } from "node:path";
import { randomBytes, pbkdf2Sync, createCipheriv } from "node:crypto";
import { gzipSync } from "node:zlib";

const [src, dest] = process.argv.slice(2);
if (!src || !dest) { console.error("usage: node tools/encrypt_queue.mjs <queue.json> <out.enc.json>"); process.exit(2); }
const password = process.env.QUEUE_PASSWORD || "";
mkdirSync(dirname(dest), { recursive: true });

if (!password) {
  writeFileSync(dest, JSON.stringify({ v: 1, available: false,
    note: "The patient queue is not published because the QUEUE_PASSWORD secret is not set in the repository." }));
  console.log("QUEUE_PASSWORD not set: wrote a placeholder, no patient data published.");
  process.exit(0);
}
if (password.length < 10) { console.error("QUEUE_PASSWORD must be at least 10 characters."); process.exit(1); }

const ITER = 310000;
const salt = randomBytes(16), iv = randomBytes(12);
const key = pbkdf2Sync(password, salt, ITER, 32, "sha256");
const plain = gzipSync(readFileSync(src));
const c = createCipheriv("aes-256-gcm", key, iv);
const ct = Buffer.concat([c.update(plain), c.final(), c.getAuthTag()]);   // Web Crypto expects the tag appended
writeFileSync(dest, JSON.stringify({ v: 1, available: true, kdf: "PBKDF2-SHA256", iter: ITER, cipher: "AES-256-GCM", gzip: true,
  salt: salt.toString("base64"), iv: iv.toString("base64"), data: ct.toString("base64") }));
console.log(`Encrypted queue written to ${dest} (${(ct.length / 1024).toFixed(1)} KB).`);
