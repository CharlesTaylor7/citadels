/** @type {import('tailwindcss').Config} */
module.exports = {
  plugins: [require("daisyui")],
  content:  [
    "citadels_server/templates/**/*", 
    "citadels_server/src/templates.rs", 
    "citadels_server/src/templates/filters.rs"
  ],
  daisyui: {
    themes: true,
  },
  theme: {
    extend: {
      borderRadius: {
        'box': 'var(--rounded-box, 1rem)',
      },
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
