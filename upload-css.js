import { createClient } from '@supabase/supabase-js'
import fs from 'node:fs';

// Create a single supabase client for interacting with your database
const supabase = createClient(process.env.SUPABASE_PROJECT_URL, process.env.SUPABASE_ADMIN_KEY);

const create = await supabase
  .storage
  .createBucket('styles', {
    public: true,
  });

if (create.error) {
  await supabase
  .storage
  .emptyBucket('styles').then(logResponse);
}

const buffer = fs.readFileSync("styles/index.min.css");
console.log(buffer);
await supabase
  .storage
  .from('styles')
  .upload('index.css', buffer, {
    contentType: 'text/css;charset=UTF-8'
  }).then(logResponse);


function logResponse({ data, error }) {
  if (data) console.log(data);
  if (error) console.error(error);
}
