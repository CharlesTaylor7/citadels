def main [] {
  roles
  districts
}

def roles [] {
  mkdir public/roles
  magick public/roles.jpeg -crop 10x3@ +repage +adjoin public/roles/%02d.webp
}
def districts [] {
  mkdir public/districts
  magick public/districts.jpeg -crop 10x5@ +repage +adjoin public/districts/%02d.webp
}
