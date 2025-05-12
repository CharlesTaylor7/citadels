/**
 * TypeScript port of the Citadels districts
 * Converted from Rust implementation
 */
import { CardSet, CardSuit } from "@/core/types";
import { ActionTag } from "@/core/actions";

export const DISTRICT_NAMES = [
  // religious
  "Temple",
  "Church",
  "Monastery",
  "Cathedral",
  "Watchtower",
  // military
  "Prison",
  "Barracks",
  "Fortress",
  // noble
  "Manor",
  "Castle",
  "Palace",
  // trade
  "Tavern",
  "Market",
  "TradingPost",
  "Docks",
  "Harbor",
  "TownHall",
  // unique
  "Smithy",
  "Laboratory",
  "SchoolOfMagic",
  "Keep",
  "DragonGate",
  "HauntedQuarter",
  "GreatWall",
  "Observatory",
  "Library",
  "Quarry",
  "Armory",
  "Factory",
  "Park",
  "Museum",
  "PoorHouse",
  "MapRoom",
  "WishingWell",
  "ImperialTreasury",
  "Framework",
  "Statue",
  "GoldMine",
  "IvoryTower",
  "Necropolis",
  "ThievesDen",
  "Theater",
  "Stables",
  "Basilica",
  "SecretVault",
  "Capitol",
  "Monument",
] as const;

export type DistrictName = (typeof DISTRICT_NAMES)[number];

export interface DistrictData {
  id: number;
  name: DistrictName;
  displayName: string;
  cost: number;
  suit: CardSuit;
  set: CardSet;
  description?: string;
  multiplicity: number;
}

export const DistrictNameUtils = {
  normal(
    id: number,
    name: DistrictName,
    set: CardSet,
    suit: CardSuit,
    cost: number,
    displayName: string,
    multiplicity: number,
  ): DistrictData {
    return {
      id,
      name,
      set,
      suit,
      cost,
      displayName,
      description: undefined,
      multiplicity,
    };
  },

  unique(
    id: number,
    name: DistrictName,
    set: CardSet,
    displayName: string,
    cost: number,
    description: string,
  ): DistrictData {
    return {
      id,
      name,
      set,
      suit: "Unique",
      cost,
      displayName,
      description,
      multiplicity: 1,
    };
  },

  /**
   * Get the data for a district
   * @param name The district name
   * @returns The district data
   */
  data(name: DistrictName): DistrictData {
    const index = DISTRICT_NAMES.indexOf(name);
    const normalLength = NORMAL_DISTRICTS.length;

    if (index < normalLength) {
      return NORMAL_DISTRICTS[index];
    } else {
      return UNIQUE_DISTRICTS[index - normalLength];
    }
  },

  /**
   * Get the action associated with a district, if any
   * @param name The district name
   * @returns The action tag, if any
   */
  action(name: DistrictName): ActionTag | undefined {
    switch (name) {
      case "Smithy":
        return "Smithy";
      case "Museum":
        return "Museum";
      case "Laboratory":
        return "Laboratory";
      case "Armory":
        return "Armory";
      default:
        return undefined;
    }
  },

};

/**
 * Normal districts in the game
 */
export const NORMAL_DISTRICTS: DistrictData[] = [
  DistrictNameUtils.normal(0, "Temple", "Base", "Religious", 1, "Temple", 3),
  DistrictNameUtils.normal(1, "Church", "Base", "Religious", 2, "Church", 3),
  DistrictNameUtils.normal(2, "Monastery", "Base", "Religious", 3, "Monastery", 3),
  DistrictNameUtils.normal(3, "Cathedral", "Base", "Religious", 5, "Cathedral", 2),

  DistrictNameUtils.normal(
    4,
    "Watchtower",
    "Base",
    "Military",
    1,
    "Watchtower",
    3,
  ),
  DistrictNameUtils.normal(5, "Prison", "Base", "Military", 2, "Prison", 3),
  DistrictNameUtils.normal(6, "Barracks", "Base", "Military", 3, "Barracks", 3),
  DistrictNameUtils.normal(7, "Fortress", "Base", "Military", 5, "Fortress", 2),

  DistrictNameUtils.normal(8, "Manor", "Base", "Noble", 3, "Manor", 5),
  DistrictNameUtils.normal(9, "Castle", "Base", "Noble", 4, "Castle", 4),
  DistrictNameUtils.normal(10, "Palace", "Base", "Noble", 5, "Palace", 3),

  DistrictNameUtils.normal(11, "Tavern", "Base", "Trade", 1, "Tavern", 5),
  DistrictNameUtils.normal(12, "Market", "Base", "Trade", 2, "Market", 4),
  DistrictNameUtils.normal(
    13,
    "TradingPost",
    "Base",
    "Trade",
    2,
    "Trading Post",
    3,
  ),
  DistrictNameUtils.normal(14, "Docks", "Base", "Trade", 3, "Docks", 3),
  DistrictNameUtils.normal(15, "Harbor", "Base", "Trade", 4, "Harbor", 3),
  DistrictNameUtils.normal(16, "TownHall", "Base", "Trade", 5, "Town Hall", 2),
];

/**
 * Unique districts in the game
 */
export const UNIQUE_DISTRICTS: DistrictData[] = [
  {
    id: 17,
    name: "Smithy",
    suit: "Unique",
    set: "Base",
    displayName: "Smithy",
    cost: 5,
    description: "Once during your turn, you may pay 2 gold to draw 3 cards.",
    multiplicity: 1,
  },
  {
    id: 18,
    name: "Laboratory",
    suit: "Unique",
    set: "Base",
    displayName: "Laboratory",
    cost: 5,
    description:
      "Once during your turn, you may discard 1 card from your hand to gain 2 gold.",
    multiplicity: 1,
  },
  {
    id: 19,
    name: "SchoolOfMagic",
    suit: "Unique",
    set: "Base",
    displayName: "School of Magic",
    cost: 6,
    description:
      "For the purpose of income, the School of Magic is treated as the district type of your choice.",
    multiplicity: 1,
  },
  {
    id: 20,
    name: "Keep",
    suit: "Unique",
    set: "Base",
    displayName: "Keep",
    cost: 3,
    description: "The Keep cannot be destroyed by the Warlord.",
    multiplicity: 1,
  },
  {
    id: 21,
    name: "DragonGate",
    suit: "Unique",
    set: "Base",
    displayName: "Dragon Gate",
    cost: 6,
    description: "At the end of the game score 2 extra points.",
    multiplicity: 1,
  },
  {
    id: 22,
    name: "HauntedQuarter",
    suit: "Unique",
    set: "Base",
    displayName: "Haunted Quarter",
    cost: 2,
    description:
      "For the purpose of final scoring, the Haunted Quarter is treated as the district type of your choice.",
    multiplicity: 1,
  },
  {
    id: 23,
    name: "GreatWall",
    suit: "Unique",
    set: "Base",
    displayName: "Great Wall",
    cost: 6,
    description:
      "The cost to destroy any of your other districts is increased by 1.",
    multiplicity: 1,
  },
  {
    id: 24,
    name: "Observatory",
    suit: "Unique",
    set: "Base",
    displayName: "Observatory",
    cost: 4,
    description:
      "If you choose to draw cards when gathering resources, you draw 3 cards instead of 2 and keep 1 of your choice.",
    multiplicity: 1,
  },
  {
    id: 25,
    name: "Library",
    suit: "Unique",
    set: "Base",
    displayName: "Library",
    cost: 6,
    description:
      "If you choose to draw cards when gathering resources, you keep both of the cards you draw.",
    multiplicity: 1,
  },
  {
    id: 26,
    name: "Quarry",
    suit: "Unique",
    set: "DarkCity",
    displayName: "Quarry",
    cost: 5,
    description:
      "You can build districts that are identical to districts in your city.",
    multiplicity: 1,
  },
  {
    id: 27,
    name: "Armory",
    suit: "Unique",
    set: "DarkCity",
    displayName: "Armory",
    cost: 3,
    description:
      "During your turn, destroy the Armory to destroy 1 district of your choice.",
    multiplicity: 1,
  },
  {
    id: 28,
    name: "Factory",
    suit: "Unique",
    set: "DarkCity",
    displayName: "Factory",
    cost: 5,
    description: "The cost to build other districts is reduced by 1 gold.",
    multiplicity: 1,
  },
  {
    id: 29,
    name: "Park",
    suit: "Unique",
    set: "DarkCity",
    displayName: "Park",
    cost: 6,
    description:
      "At the end of the game, score 2 extra points if you have no cards in your hand.",
    multiplicity: 1,
  },
  {
    id: 30,
    name: "Museum",
    suit: "Unique",
    set: "DarkCity",
    displayName: "Museum",
    cost: 4,
    description:
      "Once during your turn, you may place a card from your hand face down under the Museum. At the end of the game, score 1 extra point for each card under the Museum.",
    multiplicity: 1,
  },
  {
    id: 31,
    name: "PoorHouse",
    suit: "Unique",
    set: "DarkCity",
    displayName: "Poor House",
    cost: 4,
    description:
      "If you have no gold in your stash at the end of your turn, gain 1 gold.",
    multiplicity: 1,
  },
  {
    id: 32,
    name: "MapRoom",
    suit: "Unique",
    set: "DarkCity",
    displayName: "Map Room",
    cost: 5,
    description:
      "At the end of the game, score 1 extra point for each card in your hand.",
    multiplicity: 1,
  },
  {
    id: 33,
    name: "WishingWell",
    suit: "Unique",
    set: "DarkCity",
    displayName: "Wishing Well",
    cost: 5,
    description:
      "At the end of the game, score 1 extra point for each UNIQUE district in your city (including Wishing Well).",
    multiplicity: 1,
  },
  {
    id: 34,
    name: "ImperialTreasury",
    suit: "Unique",
    set: "DarkCity",
    displayName: "Imperial Treasury",
    cost: 5,
    description:
      "At the end of the game, score 1 extra point for each gold in your stash.",
    multiplicity: 1,
  },
  {
    id: 35,
    name: "Framework",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Framework",
    cost: 3,
    description:
      "You can build a district by destroying the Framework instead of paying that district's cost.",
    multiplicity: 1,
  },
  {
    id: 36,
    name: "Statue",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Statue",
    cost: 3,
    description:
      "If you have the crown at the end of the game, score 5 extra points.",
    multiplicity: 1,
  },
  {
    id: 37,
    name: "GoldMine",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Gold Mine",
    cost: 6,
    description:
      "If you choose to gain gold when gathering resources, gain 1 extra gold.",
    multiplicity: 1,
  },
  {
    id: 38,
    name: "IvoryTower",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Ivory Tower",
    cost: 5,
    description:
      "If the Ivory Tower is the only UNIQUE district in your city at the end of the game, score 5 extra points",
    multiplicity: 1,
  },
  {
    id: 39,
    name: "Necropolis",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Necropolis",
    cost: 5,
    description:
      "You can build the Necropolis by destroying 1 district in your city instead of paying the Necropolis' cost.",
    multiplicity: 1,
  },
  {
    id: 40,
    name: "ThievesDen",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Thieves' Den",
    cost: 6,
    description:
      "Pay some or all of the Thieves' Den cost with cards from your hand instead of gold at a rate of 1 card to 1 gold.",
    multiplicity: 1,
  },
  {
    id: 41,
    name: "Theater",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Theater",
    cost: 6,
    description:
      "At the end of each selection phase, you may exchange your chosen character card with an opponent's character card.",
    multiplicity: 1,
  },
  {
    id: 42,
    name: "Stables",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Stables",
    cost: 2,
    description:
      "Building the Stables does not count toward your building limit for the turn.",
    multiplicity: 1,
  },
  {
    id: 43,
    name: "Basilica",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Basilica",
    cost: 4,
    description:
      "At the end of the game, score 1 extra point for each district in your city with an odd-numbered cost.",
    multiplicity: 1,
  },
  {
    id: 44,
    name: "SecretVault",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Secret Vault",
    cost: 1000000, // Effectively infinite cost
    description:
      "The Secret Vault cannot be built. At the end of the game, reveal the Secret Vault from your hand to score 3 extra points.",
    multiplicity: 1,
  },
  {
    id: 45,
    name: "Capitol",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Capitol",
    cost: 5,
    description:
      "If you have at least 3 districts of the same type at the end of the game, score 3 extra points.",
    multiplicity: 1,
  },
  {
    id: 46,
    name: "Monument",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Monument",
    cost: 4,
    description:
      "You cannot build the Monument if you have 5 or more districts in your city. Treat the Monument as being 2 districts toward your completed city.",
    multiplicity: 1,
  },
];
