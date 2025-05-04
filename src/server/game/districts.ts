/**
 * TypeScript port of the Citadels districts
 * Converted from Rust implementation
 */
import { CardSuit, CARD_SUIT } from "./types";
import { ActionTag } from "./actions";
import { CardSet } from "./roles";
import { z } from "zod";

// Define the Zod enum
export const DistrictEnum = z.enum([
  // religious
  "Temple",
  "Church",
  "Monastery",
  "Cathedral",
  "Watchtower",
  // military
  "Prison",
  "Baracks",
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
]);

// Export the array of district names
export const DISTRICT_NAMES = DistrictEnum.options;

// Type representing a district name in the game
export type DistrictName = z.infer<typeof DistrictEnum>;

/**
 * Immutable data for a district
 */
export interface DistrictData {
  name: DistrictName;
  displayName: string;
  cost: number;
  suit: CardSuit;
  set: CardSet;
  description?: string;
}

export const DistrictNameUtils = {
  normal(
    name: DistrictName,
    set: CardSet,
    suit: CardSuit,
    cost: number,
    displayName: string
  ): DistrictData {
    return {
      name,
      set,
      suit,
      cost,
      displayName,
      description: undefined,
    };
  },

  unique(
    name: DistrictName,
    set: CardSet,
    displayName: string,
    cost: number,
    description: string
  ): DistrictData {
    return {
      name,
      set,
      suit: CARD_SUIT.UNIQUE,
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
      case "Baracks":
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
  DistrictNameUtils.normal("Temple", "Base", CARD_SUIT.RELIGIOUS, 1, "Temple"),
  DistrictNameUtils.normal("Church", "Base", CARD_SUIT.RELIGIOUS, 2, "Church"),
  DistrictNameUtils.normal(
    "Monastery",
    "Base",
    CARD_SUIT.RELIGIOUS,
    3,
    "Monastery"
  ),
  DistrictNameUtils.normal(
    "Cathedral",
    "Base",
    CARD_SUIT.RELIGIOUS,
    5,
    "Cathedral"
  ),

  DistrictNameUtils.normal(
    "Watchtower",
    "Base",
    CARD_SUIT.MILITARY,
    1,
    "Watchtower"
  ),
  DistrictNameUtils.normal("Prison", "Base", CARD_SUIT.MILITARY, 2, "Prison"),
  DistrictNameUtils.normal("Baracks", "Base", CARD_SUIT.MILITARY, 3, "Baracks"),
  DistrictNameUtils.normal(
    "Fortress",
    "Base",
    CARD_SUIT.MILITARY,
    5,
    "Fortress"
  ),

  DistrictNameUtils.normal("Manor", "Base", CARD_SUIT.NOBLE, 3, "Manor"),
  DistrictNameUtils.normal("Castle", "Base", CARD_SUIT.NOBLE, 4, "Castle"),
  DistrictNameUtils.normal("Palace", "Base", CARD_SUIT.NOBLE, 5, "Palace"),

  DistrictNameUtils.normal("Tavern", "Base", CARD_SUIT.TRADE, 1, "Tavern"),
  DistrictNameUtils.normal("Market", "Base", CARD_SUIT.TRADE, 2, "Market"),
  DistrictNameUtils.normal(
    "TradingPost",
    "Base",
    CARD_SUIT.TRADE,
    2,
    "Trading Post"
  ),
  DistrictNameUtils.normal("Docks", "Base", CARD_SUIT.TRADE, 3, "Docks"),
  DistrictNameUtils.normal("Harbor", "Base", CARD_SUIT.TRADE, 4, "Harbor"),
  DistrictNameUtils.normal("TownHall", "Base", CARD_SUIT.TRADE, 5, "Town Hall"),
];

/**
 * Unique districts in the game
 */
export const UNIQUE_DISTRICTS: DistrictData[] = [
  {
    name: "Smithy",
    suit: CARD_SUIT.UNIQUE,
    set: "Base",
    displayName: "Smithy",
    cost: 5,
    description: "Once during your turn, you may pay 2 gold to draw 3 cards.",
  },
  {
    name: "Laboratory",
    suit: CARD_SUIT.UNIQUE,
    set: "Base",
    displayName: "Laboratory",
    cost: 5,
    description:
      "Once during your turn, you may discard 1 card from your hand to gain 2 gold.",
  },
  {
    name: "SchoolOfMagic",
    suit: CARD_SUIT.UNIQUE,
    set: "Base",
    displayName: "School of Magic",
    cost: 6,
    description:
      "For the purpose of income, the School of Magic is treated as the district type of your choice.",
  },
  {
    name: "Keep",
    suit: CARD_SUIT.UNIQUE,
    set: "Base",
    displayName: "Keep",
    cost: 3,
    description: "The Keep cannot be destroyed by the Warlord.",
  },
  {
    name: "DragonGate",
    suit: CARD_SUIT.UNIQUE,
    set: "Base",
    displayName: "Dragon Gate",
    cost: 6,
    description: "At the end of the game score 2 extra points.",
  },
  {
    name: "HauntedQuarter",
    suit: CARD_SUIT.UNIQUE,
    set: "Base",
    displayName: "Haunted Quarter",
    cost: 2,
    description:
      "For the purpose of final scoring, the Haunted Quarter is treated as the district type of your choice.",
  },
  {
    name: "GreatWall",
    suit: CARD_SUIT.UNIQUE,
    set: "Base",
    displayName: "Great Wall",
    cost: 6,
    description:
      "The cost to destroy any of your other districts is increased by 1.",
  },
  {
    name: "Observatory",
    suit: CARD_SUIT.UNIQUE,
    set: "Base",
    displayName: "Observatory",
    cost: 4,
    description:
      "If you choose to draw cards when gathering resources, you draw 3 cards instead of 2 and keep 1 of your choice.",
  },
  {
    name: "Library",
    suit: CARD_SUIT.UNIQUE,
    set: "Base",
    displayName: "Library",
    cost: 6,
    description:
      "If you choose to draw cards when gathering resources, you keep both of the cards you draw.",
  },
  {
    name: "Quarry",
    suit: CARD_SUIT.UNIQUE,
    set: "DarkCity",
    displayName: "Quarry",
    cost: 5,
    description:
      "You can build districts that are identical to districts in your city.",
  },
  {
    name: "Armory",
    suit: CARD_SUIT.UNIQUE,
    set: "DarkCity",
    displayName: "Armory",
    cost: 3,
    description:
      "During your turn, destroy the Armory to destroy 1 district of your choice.",
  },
  {
    name: "Factory",
    suit: CARD_SUIT.UNIQUE,
    set: "DarkCity",
    displayName: "Factory",
    cost: 5,
    description: "The cost to build other districts is reduced by 1 gold.",
  },
  {
    name: "Park",
    suit: CARD_SUIT.UNIQUE,
    set: "DarkCity",
    displayName: "Park",
    cost: 6,
    description:
      "At the end of the game, score 2 extra points if you have no cards in your hand.",
  },
  {
    name: "Museum",
    suit: CARD_SUIT.UNIQUE,
    set: "DarkCity",
    displayName: "Museum",
    cost: 4,
    description:
      "Once during your turn, you may place a card from your hand face down under the Museum. At the end of the game, score 1 extra point for each card under the Museum.",
  },
  {
    name: "PoorHouse",
    suit: CARD_SUIT.UNIQUE,
    set: "DarkCity",
    displayName: "Poor House",
    cost: 4,
    description:
      "If you have no gold in your stash at the end of your turn, gain 1 gold.",
  },
  {
    name: "MapRoom",
    suit: CARD_SUIT.UNIQUE,
    set: "DarkCity",
    displayName: "Map Room",
    cost: 5,
    description:
      "At the end of the game, score 1 extra point for each card in your hand.",
  },
  {
    name: "WishingWell",
    suit: CARD_SUIT.UNIQUE,
    set: "DarkCity",
    displayName: "Wishing Well",
    cost: 5,
    description:
      "At the end of the game, score 1 extra point for each UNIQUE district in your city (including Wishing Well).",
  },
  {
    name: "ImperialTreasury",
    suit: CARD_SUIT.UNIQUE,
    set: "DarkCity",
    displayName: "Imperial Treasury",
    cost: 5,
    description:
      "At the end of the game, score 1 extra point for each gold in your stash.",
  },
  {
    name: "Framework",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Framework",
    cost: 3,
    description:
      "You can build a district by destroying the Framework instead of paying that district's cost.",
  },
  {
    name: "Statue",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Statue",
    cost: 3,
    description:
      "If you have the crown at the end of the game, score 5 extra points.",
  },
  {
    name: "GoldMine",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Gold Mine",
    cost: 6,
    description:
      "If you choose to gain gold when gathering resources, gain 1 extra gold.",
  },
  {
    name: "IvoryTower",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Ivory Tower",
    cost: 5,
    description:
      "If the Ivory Tower is the only UNIQUE district in your city at the end of the game, score 5 extra points",
  },
  {
    name: "Necropolis",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Necropolis",
    cost: 5,
    description:
      "You can build the Necropolis by destroying 1 district in your city instead of paying the Necropolis' cost.",
  },
  {
    name: "ThievesDen",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Thieves' Den",
    cost: 6,
    description:
      "Pay some or all of the Thieves' Den cost with cards from your hand instead of gold at a rate of 1 card to 1 gold.",
  },
  {
    name: "Theater",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Theater",
    cost: 6,
    description:
      "At the end of each selection phase, you may exchange your chosen character card with an opponent's character card.",
  },
  {
    name: "Stables",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Stables",
    cost: 2,
    description:
      "Building the Stables does not count toward your building limit for the turn.",
  },
  {
    name: "Basilica",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Basilica",
    cost: 4,
    description:
      "At the end of the game, score 1 extra point for each district in your city with an odd-numbered cost.",
  },
  {
    name: "SecretVault",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Secret Vault",
    cost: 1000000,
    description:
      "The Secret Vault cannot be built. At the end of the game, reveal the Secret Vault from your hand to score 3 extra points.",
  },
  {
    name: "Capitol",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Capitol",
    cost: 5,
    description:
      "If you have at least 3 districts of the same type at the end of the game, score 3 extra points.",
  },
  {
    name: "Monument",
    suit: CARD_SUIT.UNIQUE,
    set: "Citadels2016",
    displayName: "Monument",
    cost: 4,
    description:
      "You cannot build the Monument if you have 5 or more districts in your city. Treat the Monument as being 2 districts toward your completed city.",
  },
];
