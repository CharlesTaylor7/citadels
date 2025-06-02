export const CARD_SUITS = [
  "Trade",
  "Religious",
  "Military",
  "Noble",
  "Unique",
] as const;

export type CardSuit = (typeof CARD_SUITS)[number];

export const CARD_SETS = ["Base", "DarkCity", "Citadels2016"] as const;

export type CardSet = (typeof CARD_SETS)[number];

export type BlackmailMarker = { tag: "Blackmail"; flowered: boolean };
export type WarrantMarker = { tag: "Warrant"; signed: boolean };

export type Marker =
  | { tag: "Discarded" | "Killed" | "Bewitched" | "Robbed" }
  | BlackmailMarker
  | WarrantMarker;

export type PlayerId = number;
export type Cost = number;
