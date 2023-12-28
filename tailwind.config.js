/** @type {import('tailwindcss').Config} */
module.exports = {
  content: { 
    files: ["src/**/*.rs", "templates/**/*.j2", "templates/**/*.html"],
  },
  theme: {
    extend: {
    }
  },
  plugins: [],
}
