
def main [] {
  let prod_env = open .env.prod
  | split column "=" -n 2
  | transpose -r
  | into record

  with-env $prod_env { sqlx migrate run }
}
 
