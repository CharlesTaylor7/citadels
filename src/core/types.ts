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

export type BlackmailMarker = { type: "Blackmail"; flowered: boolean };
export type WarrantMarker = { type: "Warrant"; signed: boolean };

export type Marker =
  | { type: "Discarded" | "Killed" | "Bewitched" | "Robbed" }
  | BlackmailMarker
  | WarrantMarker;

type MarkerType = Marker["type"];

export type DistrictName = string;
export type PlayerId = number;
export type Cost = number;
