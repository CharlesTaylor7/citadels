import { createClient } from '@supabase/supabase-js'
import fs from 'node:fs';

// Create a single supabase client for interacting with your database
const url = "https://ryvsflpspddwwacxrnst.supabase.co";

const supabase = createClient(url, process.env.PROD_SUPABASE_SERVICE_ROLE_KEY);

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
