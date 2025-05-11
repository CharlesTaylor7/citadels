import { DistrictName } from "@/core/districts";
import { RoleName } from "@/core/roles";
import { CardSuit } from "@/core/types";
import { GameConfig } from "@/core/config";

export const ACTION_TAGS = [
  "DraftPick",
  "DraftDiscard",
  "GatherResourceGold",
  "GatherResourceCards",
  "GatherCardsPick",
  "Build",
  "EndTurn",
  "GoldFromNobility",
  "GoldFromReligion",
  "GoldFromTrade",
  "GoldFromMilitary",
  "CardsFromNobility",
  "MerchantGainOneGold",
  "ArchitectGainCards",
  "TakeCrown",
  "Assassinate",
  "Steal",
  "Magic",
  "WarlordDestroy",
  "Beautify",
  "ScholarReveal",
  "ScholarPick",
  "EmperorGiveCrown",
  "EmperorHeirGiveCrown",
  "ResourcesFromReligion",
  "TakeFromRich",
  "CardsFromReligion",
  "Bewitch",
  "SendWarrants",
  "Blackmail",
  "PayBribe",
  "IgnoreBlackmail",
  "RevealWarrant",
  "RevealBlackmail",
  "Pass",
  "MarshalSeize",
  "CollectTaxes",
  "DiplomatTrade",
  "Spy",
  "SpyAcknowledge",
  "QueenGainGold",
  "NavigatorGain",
  "SeerTake",
  "SeerDistribute",
  "WizardPeek",
  "WizardPick",
  "Smithy",
  "Laboratory",
  "Armory",
  "Museum",
  "Theater",
  "TheaterPass",
  "PatricianTakeCrown",
  "CardinalExchange",
] as const;

/**
 * Type representing an action tag in the game
 */
export type ActionTag = (typeof ACTION_TAGS)[number];

/**
 * Resources that can be gained in the game
 */
export const RESOURCES = ["Gold", "Cards"] as const;

export type Resource = (typeof RESOURCES)[number];

/**
 * Target for a district in a player's city
 */
export interface CityDistrictTarget {
  player: PlayerName;
  district: DistrictName;
  beautified: boolean;
}

/**
 * Utility functions for CityDistrictTarget
 */
export const CityDistrictTargetUtils = {
  /**
   * Calculate the effective cost of a district, including beautification
   * @param target The city district target
   * @returns The effective cost
   */
  effectiveCost(target: CityDistrictTarget): number {
    // In the original, this calls district.data().cost
    // We'll need to implement this once we have the district data available
    const baseDistrictCost = 0; // Placeholder - replace with actual implementation
    return baseDistrictCost + (target.beautified ? 1 : 0);
  },
};

/**
 * Methods for building districts
 */
export type BuildMethod =
  | { buildMethod: "Regular"; district: DistrictName }
  | { buildMethod: "Framework"; district: DistrictName }
  | { buildMethod: "Necropolis"; sacrifice: CityDistrictTarget }
  | { buildMethod: "ThievesDen"; discard: DistrictName[] }
  | {
      buildMethod: "Cardinal";
      district: DistrictName;
      discard: DistrictName[];
      player: PlayerName;
    };

/**
 * Methods for wizard actions
 */
export type WizardMethod =
  | { wizardMethod: "Pick"; district: DistrictName }
  | { wizardMethod: "Build"; district: DistrictName }
  | { wizardMethod: "Framework"; district: DistrictName }
  | { wizardMethod: "Necropolis"; sacrifice: CityDistrictTarget }
  | { wizardMethod: "ThievesDen"; discard: DistrictName[] };

type PlayerName = string;
/**
 * Actions for the magician
 */
export type MagicianAction =
  | { player: PlayerName }
  | { district: DistrictName[] };

export type Action = Readonly<GameStartAction | PlayerAction>;

export type GameStartAction = {
  action: "GameStart";
} & GameConfig;
/**
 * All possible player actions in the game
 *
 */
export type PlayerAction =
  | { action: "DraftPick"; role: RoleName }
  | { action: "DraftDiscard"; role: RoleName }
  | { action: "GatherResourceGold" }
  | { action: "GatherResourceCards" }
  | { action: "GatherCardsPick"; district: DistrictName }
  | { action: "Build"; buildMethod: BuildMethod }
  | { action: "EndTurn" }
  | { action: "GoldFromNobility" }
  | { action: "GoldFromReligion" }
  | { action: "GoldFromTrade" }
  | { action: "GoldFromMilitary" }
  | { action: "CardsFromNobility" }
  | { action: "MerchantGainOneGold" }
  | { action: "ArchitectGainCards" }
  | { action: "TakeCrown" }
  | { action: "Assassinate"; role: RoleName }
  | { action: "Steal"; role: RoleName }
  | { action: "Magic"; magicianAction: MagicianAction }
  | { action: "WarlordDestroy"; district: CityDistrictTarget }
  | { action: "Beautify"; district: CityDistrictTarget }
  | { action: "ScholarReveal" }
  | { action: "ScholarPick"; district: DistrictName }
  | { action: "EmperorGiveCrown"; player: PlayerName; resource: Resource }
  | { action: "EmperorHeirGiveCrown"; player: PlayerName }
  | { action: "ResourcesFromReligion"; gold: number; cards: number }
  | { action: "TakeFromRich"; player: PlayerName }
  | { action: "CardsFromReligion" }
  | { action: "Bewitch"; role: RoleName }
  | { action: "SendWarrants"; signed: RoleName; unsigned: [RoleName, RoleName] }
  | { action: "Blackmail"; flowered: RoleName; unmarked: RoleName }
  | { action: "PayBribe" }
  | { action: "IgnoreBlackmail" }
  | { action: "RevealWarrant" }
  | { action: "RevealBlackmail" }
  | { action: "Pass" }
  | { action: "MarshalSeize"; district: CityDistrictTarget }
  | { action: "CollectTaxes" }
  | {
      action: "DiplomatTrade";
      district: CityDistrictTarget;
      theirs: CityDistrictTarget;
    }
  | { action: "Spy"; player: PlayerName; suit: CardSuit }
  | { action: "SpyAcknowledge" }
  | { action: "QueenGainGold" }
  | { action: "NavigatorGain"; resource: Resource }
  | { action: "SeerTake" }
  | { action: "SeerDistribute"; seer: [PlayerName, DistrictName][] }
  | { action: "WizardPeek"; player: PlayerName }
  | { action: "WizardPick"; wizardMethod: WizardMethod }
  | { action: "Smithy" }
  | { action: "Laboratory"; district: DistrictName }
  | { action: "Armory"; district: CityDistrictTarget }
  | { action: "Museum"; district: DistrictName }
  | { action: "Theater"; role: RoleName; player: PlayerName }
  | { action: "TheaterPass" }
  | { action: "PatricianTakeCrown" }
  | { action: "CardinalExchange" };

export const ActionTagUtils = {
  isResourceGathering(tag: ActionTag): boolean {
    return [
      "GatherResourceGold",
      "GatherResourceCards",
      "GatherCardsPick",
    ].includes(tag);
  },

  isRequired(tag: ActionTag): boolean {
    return [
      "DraftPick",
      "GatherResourceGold",
      "GatherResourceCards",
      "EndTurn",
    ].includes(tag);
  },

  label(tag: ActionTag): string {
    switch (tag) {
      case "GoldFromTrade":
        return "Gain gold from Trade";
      case "GoldFromReligion":
        return "Gain gold from Religious";
      case "GoldFromMilitary":
        return "Gain gold from Military";
      case "GoldFromNobility":
        return "Gain gold from Noble";
      case "CardsFromNobility":
        return "Gain cards from Noble";
      case "CardsFromReligion":
        return "Gain cards from Religious";
      case "ResourcesFromReligion":
        return "Gain resources from Religious";
      case "DraftPick":
        return "Pick";
      case "DraftDiscard":
        return "Discard";
      case "Build":
        return "Build";
      case "EndTurn":
        return "End turn";
      case "Assassinate":
        return "Assassinate";
      case "Steal":
        return "Steal";
      case "Magic":
        return "Magic";
      case "TakeCrown":
        return "Take Crown";
      case "MerchantGainOneGold":
        return "Gain 1 extra gold";
      case "ArchitectGainCards":
        return "Gain 2 extra cards";
      case "SendWarrants":
        return "Send Warrants";
      case "WarlordDestroy":
        return "Destroy";
      case "Beautify":
        return "Beautify";
      case "ScholarReveal":
        return "Draw 7, pick 1";
      case "ScholarPick":
        return "Pick";
      case "Museum":
        return "Museum";
      case "Smithy":
        return "Smithy";
      case "Theater":
        return "Theater";
      case "RevealWarrant":
        return "Confiscate";
      case "Pass":
        return "Pass";
      case "RevealBlackmail":
        return "Reveal Blackmail";
      case "PayBribe":
        return "Pay bribe";
      case "IgnoreBlackmail":
        return "Ignore blackmail";
      case "Armory":
        return "Armory";
      case "Laboratory":
        return "Laboratory";
      case "NavigatorGain":
        return "Navigator";
      case "QueenGainGold":
        return "Queen";
      case "TakeFromRich":
        return "Take 1 gold from the richest";
      case "SeerTake":
        return "Seer";
      case "WizardPeek":
        return "Wizard";
      case "MarshalSeize":
        return "Seize";
      case "DiplomatTrade":
        return "Trade";
      case "CollectTaxes":
        return "Collect Taxes";
      case "Bewitch":
        return "Bewitch";
      case "EmperorGiveCrown":
        return "Grant Crown";
      case "EmperorHeirGiveCrown":
        return "Grant Crown";
      case "Blackmail":
        return "Blackmail";
      case "Spy":
        return "Spy";
      case "GatherCardsPick":
        return "Pick";
      case "TheaterPass":
        return "Pass";
      case "SpyAcknowledge":
        return "Acknowledge";
      case "SeerDistribute":
        return "Distribute";
      case "WizardPick":
        return "Pick";
      case "PatricianTakeCrown":
        return "Take Crown";
      case "CardinalExchange":
        return "Exchange";
      default:
        return tag;
    }
  },
};
