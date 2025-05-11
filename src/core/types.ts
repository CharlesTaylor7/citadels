export const CARD_SUITS = [
  "Trade",
  "Religious",
  "Military",
  "Noble",
  "Unique",
] as const;

export type CardSuit = (typeof CARD_SUITS)[number];

/**
 * Type for card sets
 */
export const CARD_SETS = [
  "Base",
  "DarkCity",
  "Citadels2016",
  "Custom",
] as const;

export type CardSet = (typeof CARD_SETS)[number];

export type BlackmailMarker = { type: "Blackmail"; flowered: boolean };
export type WarrantMarker = { type: "Warrant"; signed: boolean };

export type Marker =
  | { type: "Discarded" | "Killed" | "Bewitched" | "Robbed" }
  | BlackmailMarker
  | WarrantMarker;

type MarkerType = Marker["type"];

export const MarkerUtils = {
  isWarrant(marker: Marker): marker is WarrantMarker {
    return marker.type === "Warrant";
  },

  isBlackmail(marker: Marker): marker is BlackmailMarker {
    return marker.type === "Blackmail";
  },

  createWarrant(signed: boolean = false): WarrantMarker {
    return { type: "Warrant", signed };
  },

  createBlackmail(flowered: boolean = false): BlackmailMarker {
    return { type: "Blackmail", flowered };
  },
};

export type DistrictName = string;
export type PlayerId = number;
export type Cost = number;
