/**
 * TypeScript port of the Citadels districts
 * Converted from Rust implementation
 */
import { CardSet, CardSuit } from "@/core/types";
import { ActionTag } from "./actions";

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
}

export const DistrictNameUtils = {
  normal(
    id: number,
    name: DistrictName,
    set: CardSet,
    suit: CardSuit,
    cost: number,
    displayName: string,
  ): DistrictData {
    return {
      id,
      name,
      set,
      suit,
      cost,
      displayName,
      description: undefined,
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

  /**
   * Get the number of copies of a district in the deck
   * @param name The district name
   * @returns The number of copies
   */
  multiplicity(name: DistrictName): number {
    switch (name) {
      case "Palace":
        return 3;
      case "Castle":
        return 4;
      case "Manor":
        return 5;

      case "Fortress":
        return 2;
      case "Barracks":
        return 3;
      case "Prison":
        return 3;
      case "Watchtower":
        return 3;

      case "Cathedral":
        return 2;
      case "Monastery":
        return 3;
      case "Church":
        return 3;
      case "Temple":
        return 3;

      case "TownHall":
        return 2;
      case "Harbor":
        return 3;
      case "Docks":
        return 3;
      case "Market":
        return 4;
      case "TradingPost":
        return 3;
      case "Tavern":
        return 5;
      default:
        // All unique districts have multiplicity 1
        return 1;
    }
  },
};

/**
 * Normal districts in the game
 */
export const NORMAL_DISTRICTS: DistrictData[] = [
  DistrictNameUtils.normal(0, "Temple", "Base", "Religious", 1, "Temple"),
  DistrictNameUtils.normal(1, "Church", "Base", "Religious", 2, "Church"),
  DistrictNameUtils.normal(2, "Monastery", "Base", "Religious", 3, "Monastery"),
  DistrictNameUtils.normal(3, "Cathedral", "Base", "Religious", 5, "Cathedral"),

  DistrictNameUtils.normal(
    4,
    "Watchtower",
    "Base",
    "Military",
    1,
    "Watchtower",
  ),
  DistrictNameUtils.normal(5, "Prison", "Base", "Military", 2, "Prison"),
  DistrictNameUtils.normal(6, "Barracks", "Base", "Military", 3, "Barracks"),
  DistrictNameUtils.normal(7, "Fortress", "Base", "Military", 5, "Fortress"),

  DistrictNameUtils.normal(8, "Manor", "Base", "Noble", 3, "Manor"),
  DistrictNameUtils.normal(9, "Castle", "Base", "Noble", 4, "Castle"),
  DistrictNameUtils.normal(10, "Palace", "Base", "Noble", 5, "Palace"),

  DistrictNameUtils.normal(11, "Tavern", "Base", "Trade", 1, "Tavern"),
  DistrictNameUtils.normal(12, "Market", "Base", "Trade", 2, "Market"),
  DistrictNameUtils.normal(
    13,
    "TradingPost",
    "Base",
    "Trade",
    2,
    "Trading Post",
  ),
  DistrictNameUtils.normal(14, "Docks", "Base", "Trade", 3, "Docks"),
  DistrictNameUtils.normal(15, "Harbor", "Base", "Trade", 4, "Harbor"),
  DistrictNameUtils.normal(16, "TownHall", "Base", "Trade", 5, "Town Hall"),
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
  },
  {
    id: 20,
    name: "Keep",
    suit: "Unique",
    set: "Base",
    displayName: "Keep",
    cost: 3,
    description: "The Keep cannot be destroyed by the Warlord.",
  },
  {
    id: 21,
    name: "DragonGate",
    suit: "Unique",
    set: "Base",
    displayName: "Dragon Gate",
    cost: 6,
    description: "At the end of the game score 2 extra points.",
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
  },
  {
    id: 28,
    name: "Factory",
    suit: "Unique",
    set: "DarkCity",
    displayName: "Factory",
    cost: 5,
    description: "The cost to build other districts is reduced by 1 gold.",
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
  },
  {
    id: 44,
    name: "SecretVault",
    suit: "Unique",
    set: "Citadels2016",
    displayName: "Secret Vault",
    cost: 1000000,
    description:
      "The Secret Vault cannot be built. At the end of the game, reveal the Secret Vault from your hand to score 3 extra points.",
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
  },
];
