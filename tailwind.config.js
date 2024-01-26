/** @type {import('tailwindcss').Config} */
module.exports = {
  plugins: [require("daisyui")],
  content: { 
    files: ["templates/**/*", "src/templates.rs", "src/templates/filters.rs"],
  },
  daisyui: {
    themes: [
      "light",
      "dark",
      "cupcake",
      "bumblebee",
      "emerald",
      "corporate",
      "synthwave",
      "retro",
      "cyberpunk",
      "valentine",
      "halloween",
      "garden",
      "forest",
      "aqua",
      "lofi",
      "pastel",
      "fantasy",
      "wireframe",
      "black",
      "luxury",
      "dracula",
      "cmyk",
      "autumn",
      "business",
      "acid",
      "lemonade",
      "night",
      "coffee",
      "winter",
      "dim",
      "nord",
      "sunset",
    ],
  },
  theme: {
    extend: {
      borderWidth: {
        '3': '3px',
      },
       backgroundImage: {
        'gradient-radial': 'radial-gradient(var(--tw-gradient-stops))',
      },
      colors: {
        'suit-trade': "rgb(104, 189, 126)",
        'suit-military': "rgb(223, 71, 71)",
        'suit-religious': "rgb(75, 203, 214)",
        'suit-noble':   "rgb(224, 189, 22)", 
        'suit-unique': "rgb(169, 107, 244)",
      }
    }
  },
}
