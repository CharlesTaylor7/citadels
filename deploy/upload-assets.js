import { createClient } from "@supabase/supabase-js";
import fs from "node:fs";
import path from "node:path";
import process from "node:process";

// Create a single supabase client for interacting with your database
const url = "https://ryvsflpspddwwacxrnst.supabase.co";

const supabase = createClient(url, process.env.PROD_SUPABASE_SERVICE_ROLE_KEY);

await supabase.storage
  .createBucket("assets", {
    public: true,
  });

// refresh styles
await supabase.storage
  .from("assets")
  .remove(["styles/index.css"])

uploadDir("public");

function uploadDir(dir) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    if (entry.isDirectory()) {
      uploadDir(path.join(dir, entry.name));
    } else {
      let filePath = path.join(entry.parentPath, entry.name);
      let contents = fs.readFileSync(filePath);
      supabase.storage
        .from("assets")
        .upload(filePath.substring(7), contents, {
          contentType: mimeType(filePath),
        });
    }
  }
}

function mimeType(fileName) {
  const ext = path.extname(fileName);
  switch (ext) {
    case ".txt":
      return "text/plain;charset=UTF-8";
    case ".css":
      return "text/css;charset=UTF-8";
    case ".mp3":
      return "audio/mpeg";
    case ".wav":
      return "audio/wav";
    case ".png":
      return "image/x-png";
    case ".jpeg":
      return "image/jpeg";
    case ".jpg":
      return "image/jpeg";
    case ".svg":
      return "image/svg+xml";
    case ".pdf":
      return "application/pdf";
    case ".js":
      return "application/javascript";
    default:
      throw new Error(`Unexpected file ext: ${ext}`);
  }
}
