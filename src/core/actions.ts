import { z } from "zod";
import { DistrictName, DISTRICT_NAMES } from "@/core/districts";
import { RoleName, ROLE_NAMES } from "@/core/roles";
import { CardSuit, CARD_SUITS } from "@/core/types";
import { GameConfig } from "@/core/config";

// types
export type ActionTag = PlayerAction["action"];
export const RESOURCES = ["Gold", "Cards"] as const;
export type Resource = (typeof RESOURCES)[number];
export type CityDistrictTarget = z.infer<typeof CityDistrictTargetSchema>;
export type BuildMethod = z.infer<typeof BuildMethodSchema>;
export type WizardMethod = z.infer<typeof WizardMethodSchema>;
export type MagicianAction = z.infer<typeof MagicianActionSchema>;
export type Action = Readonly<GameStartAction | PlayerAction>;
export interface GameStartAction extends GameConfig {
  action: "GameStart";
}

export type PlayerAction = z.infer<typeof PlayerActionSchema>;

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
      case "ScholarReveal":
        return "Draw 7, pick 1";
      case "ScholarPick":
        return "Pick";
      case "RevealWarrant":
        return "Confiscate";
      case "RevealBlackmail":
        return "Reveal Blackmail";
      case "PayBribe":
        return "Pay bribe";
      case "IgnoreBlackmail":
        return "Ignore blackmail";
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
      case "EmperorGiveCrown":
        return "Grant Crown";
      case "EmperorHeirGiveCrown":
        return "Grant Crown";
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

// Zod Schemas
const DistrictNameSchema = z.enum(DISTRICT_NAMES);
const RoleNameSchema = z.enum(ROLE_NAMES);
const CardSuitSchema = z.enum(CARD_SUITS);
const ResourceSchema = z.enum(RESOURCES);

const PlayerNameSchema = z.string();

/**
 * Target for a district in a player's city
 */
const CityDistrictTargetSchema = z.object({
  player: PlayerNameSchema,
  district: DistrictNameSchema,
  beautified: z.boolean(),
});

const BuildMethodSchema = z.discriminatedUnion("buildMethod", [
  z.object({ buildMethod: z.literal("Regular"), district: DistrictNameSchema }),
  z.object({
    buildMethod: z.literal("Framework"),
    district: DistrictNameSchema,
  }),
  z.object({
    buildMethod: z.literal("Necropolis"),
    sacrifice: CityDistrictTargetSchema,
  }),
  z.object({
    buildMethod: z.literal("ThievesDen"),
    discard: z.array(DistrictNameSchema),
  }),
  z.object({
    buildMethod: z.literal("Cardinal"),
    district: DistrictNameSchema,
    discard: z.array(DistrictNameSchema),
    player: PlayerNameSchema,
  }),
]);

const WizardMethodSchema = z.discriminatedUnion("wizardMethod", [
  z.object({ wizardMethod: z.literal("Pick"), district: DistrictNameSchema }),
  z.object({ wizardMethod: z.literal("Build"), district: DistrictNameSchema }),
  z.object({
    wizardMethod: z.literal("Framework"),
    district: DistrictNameSchema,
  }),
  z.object({
    wizardMethod: z.literal("Necropolis"),
    sacrifice: CityDistrictTargetSchema,
  }),
  z.object({
    wizardMethod: z.literal("ThievesDen"),
    discard: z.array(DistrictNameSchema),
  }),
]);

const MagicianActionSchema = z.union([
  z.object({ player: PlayerNameSchema }),
  z.object({ district: z.array(DistrictNameSchema) }),
]);

const DraftPickPlayerActionSchema = z.object({
  action: z.literal("DraftPick"),
  role: RoleNameSchema,
});
const DraftDiscardPlayerActionSchema = z.object({
  action: z.literal("DraftDiscard"),
  role: RoleNameSchema,
});
const GatherResourceGoldPlayerActionSchema = z.object({
  action: z.literal("GatherResourceGold"),
});
const GatherResourceCardsPlayerActionSchema = z.object({
  action: z.literal("GatherResourceCards"),
});
const BuildPlayerActionSchema = z.object({
  action: z.literal("Build"),
  buildMethod: BuildMethodSchema,
});
const EndTurnPlayerActionSchema = z.object({ action: z.literal("EndTurn") });
const GoldFromNobilityPlayerActionSchema = z.object({
  action: z.literal("GoldFromNobility"),
});
const GoldFromReligionPlayerActionSchema = z.object({
  action: z.literal("GoldFromReligion"),
});
const GoldFromTradePlayerActionSchema = z.object({
  action: z.literal("GoldFromTrade"),
});
const GoldFromMilitaryPlayerActionSchema = z.object({
  action: z.literal("GoldFromMilitary"),
});
const CardsFromNobilityPlayerActionSchema = z.object({
  action: z.literal("CardsFromNobility"),
});
const MerchantGainOneGoldPlayerActionSchema = z.object({
  action: z.literal("MerchantGainOneGold"),
});
const ArchitectGainCardsPlayerActionSchema = z.object({
  action: z.literal("ArchitectGainCards"),
});
const TakeCrownPlayerActionSchema = z.object({
  action: z.literal("TakeCrown"),
});
const AssassinatePlayerActionSchema = z.object({
  action: z.literal("Assassinate"),
  role: RoleNameSchema,
});
const StealPlayerActionSchema = z.object({
  action: z.literal("Steal"),
  role: RoleNameSchema,
});
const MagicPlayerActionSchema = z.object({
  action: z.literal("Magic"),
  magicianAction: MagicianActionSchema,
});
const WarlordDestroyPlayerActionSchema = z.object({
  action: z.literal("WarlordDestroy"),
  district: CityDistrictTargetSchema,
});
const BeautifyPlayerActionSchema = z.object({
  action: z.literal("Beautify"),
  district: CityDistrictTargetSchema,
});
const ScholarRevealPlayerActionSchema = z.object({
  action: z.literal("ScholarReveal"),
});
const ScholarPickPlayerActionSchema = z.object({
  action: z.literal("ScholarPick"),
  district: DistrictNameSchema,
});
const EmperorGiveCrownPlayerActionSchema = z.object({
  action: z.literal("EmperorGiveCrown"),
  player: PlayerNameSchema,
  resource: ResourceSchema,
});
const EmperorHeirGiveCrownPlayerActionSchema = z.object({
  action: z.literal("EmperorHeirGiveCrown"),
  player: PlayerNameSchema,
});
const ResourcesFromReligionPlayerActionSchema = z.object({
  action: z.literal("ResourcesFromReligion"),
  gold: z.number(),
  cards: z.number(),
});
const TakeFromRichPlayerActionSchema = z.object({
  action: z.literal("TakeFromRich"),
  player: PlayerNameSchema,
});
const CardsFromReligionPlayerActionSchema = z.object({
  action: z.literal("CardsFromReligion"),
});
const BewitchPlayerActionSchema = z.object({
  action: z.literal("Bewitch"),
  role: RoleNameSchema,
});
const SendWarrantsPlayerActionSchema = z.object({
  action: z.literal("SendWarrants"),
  signed: RoleNameSchema,
  unsigned: z.tuple([RoleNameSchema, RoleNameSchema]),
});
const BlackmailPlayerActionSchema = z.object({
  action: z.literal("Blackmail"),
  flowered: RoleNameSchema,
  unmarked: RoleNameSchema,
});
const PayBribePlayerActionSchema = z.object({ action: z.literal("PayBribe") });
const IgnoreBlackmailPlayerActionSchema = z.object({
  action: z.literal("IgnoreBlackmail"),
});
const RevealWarrantPlayerActionSchema = z.object({
  action: z.literal("RevealWarrant"),
});
const RevealBlackmailPlayerActionSchema = z.object({
  action: z.literal("RevealBlackmail"),
});
const PassPlayerActionSchema = z.object({ action: z.literal("Pass") });
const MarshalSeizePlayerActionSchema = z.object({
  action: z.literal("MarshalSeize"),
  district: CityDistrictTargetSchema,
});
const CollectTaxesPlayerActionSchema = z.object({
  action: z.literal("CollectTaxes"),
});
const DiplomatTradePlayerActionSchema = z.object({
  action: z.literal("DiplomatTrade"),
  district: CityDistrictTargetSchema,
  theirs: CityDistrictTargetSchema,
});
const SpyPlayerActionSchema = z.object({
  action: z.literal("Spy"),
  player: PlayerNameSchema,
  suit: CardSuitSchema,
});
const SpyAcknowledgePlayerActionSchema = z.object({
  action: z.literal("SpyAcknowledge"),
});
const QueenGainGoldPlayerActionSchema = z.object({
  action: z.literal("QueenGainGold"),
});
const NavigatorGainPlayerActionSchema = z.object({
  action: z.literal("NavigatorGain"),
  resource: ResourceSchema,
});
const SeerTakePlayerActionSchema = z.object({ action: z.literal("SeerTake") });
const SeerDistributePlayerActionSchema = z.object({
  action: z.literal("SeerDistribute"),
  seer: z.array(z.tuple([PlayerNameSchema, DistrictNameSchema])),
});
const WizardPeekPlayerActionSchema = z.object({
  action: z.literal("WizardPeek"),
  player: PlayerNameSchema,
});
const WizardPickPlayerActionSchema = z.object({
  action: z.literal("WizardPick"),
  wizardMethod: WizardMethodSchema,
});
const SmithyPlayerActionSchema = z.object({ action: z.literal("Smithy") });
const LaboratoryPlayerActionSchema = z.object({
  action: z.literal("Laboratory"),
  district: DistrictNameSchema,
});
const ArmoryPlayerActionSchema = z.object({
  action: z.literal("Armory"),
  district: CityDistrictTargetSchema,
});
const MuseumPlayerActionSchema = z.object({
  action: z.literal("Museum"),
  district: DistrictNameSchema,
});
const TheaterPlayerActionSchema = z.object({
  action: z.literal("Theater"),
  role: RoleNameSchema,
  player: PlayerNameSchema,
});
const TheaterPassPlayerActionSchema = z.object({
  action: z.literal("TheaterPass"),
});
const PatricianTakeCrownPlayerActionSchema = z.object({
  action: z.literal("PatricianTakeCrown"),
});
const CardinalExchangePlayerActionSchema = z.object({
  action: z.literal("CardinalExchange"),
});

export const PlayerActionSchema = z.discriminatedUnion("action", [
  DraftPickPlayerActionSchema,
  DraftDiscardPlayerActionSchema,
  GatherResourceGoldPlayerActionSchema,
  GatherResourceCardsPlayerActionSchema,
  z.object({
    action: z.literal("GatherCardsPick"),
    district: DistrictNameSchema,
  }),
  BuildPlayerActionSchema,
  EndTurnPlayerActionSchema,
  GoldFromNobilityPlayerActionSchema,
  GoldFromReligionPlayerActionSchema,
  GoldFromTradePlayerActionSchema,
  GoldFromMilitaryPlayerActionSchema,
  CardsFromNobilityPlayerActionSchema,
  MerchantGainOneGoldPlayerActionSchema,
  ArchitectGainCardsPlayerActionSchema,
  TakeCrownPlayerActionSchema,
  AssassinatePlayerActionSchema,
  StealPlayerActionSchema,
  MagicPlayerActionSchema,
  WarlordDestroyPlayerActionSchema,
  BeautifyPlayerActionSchema,
  ScholarRevealPlayerActionSchema,
  ScholarPickPlayerActionSchema,
  EmperorGiveCrownPlayerActionSchema,
  EmperorHeirGiveCrownPlayerActionSchema,
  ResourcesFromReligionPlayerActionSchema,
  TakeFromRichPlayerActionSchema,
  CardsFromReligionPlayerActionSchema,
  BewitchPlayerActionSchema,
  SendWarrantsPlayerActionSchema,
  BlackmailPlayerActionSchema,
  PayBribePlayerActionSchema,
  IgnoreBlackmailPlayerActionSchema,
  RevealWarrantPlayerActionSchema,
  RevealBlackmailPlayerActionSchema,
  PassPlayerActionSchema,
  MarshalSeizePlayerActionSchema,
  CollectTaxesPlayerActionSchema,
  DiplomatTradePlayerActionSchema,
  SpyPlayerActionSchema,
  SpyAcknowledgePlayerActionSchema,
  QueenGainGoldPlayerActionSchema,
  NavigatorGainPlayerActionSchema,
  SeerTakePlayerActionSchema,
  SeerDistributePlayerActionSchema,
  WizardPeekPlayerActionSchema,
  WizardPickPlayerActionSchema,
  SmithyPlayerActionSchema,
  LaboratoryPlayerActionSchema,
  ArmoryPlayerActionSchema,
  MuseumPlayerActionSchema,
  TheaterPlayerActionSchema,
  TheaterPassPlayerActionSchema,
  PatricianTakeCrownPlayerActionSchema,
  CardinalExchangePlayerActionSchema,
]);
