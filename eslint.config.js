import js from "@eslint/js";
import reactHooks from "eslint-plugin-react-hooks";
import noTypeAssertion from "eslint-plugin-no-type-assertion";
import tseslint from "typescript-eslint";

export default tseslint.config({
  extends: [js.configs.recommended, ...tseslint.configs.recommended],
  files: ["**/*.{ts,tsx}"],
  languageOptions: {
    ecmaVersion: 2020,
  },
  plugins: {
    "react-hooks": reactHooks,
    "no-type-assertion": noTypeAssertion,
  },
  rules: {
    ...reactHooks.configs.recommended.rules,
    "no-type-assertion/no-type-assertion": "error",
    "@typescript-eslint/no-empty-object-type": "off",
    "@typescript-eslint/no-unused-vars": "off",
    "@typescript-eslint/no-redeclare": [
      "error",
      { ignoreDeclarationMerge: false },
    ],
  },
});
