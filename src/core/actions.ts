import { z } from "zod";
import { DISTRICT_NAMES } from "@/core/districts";
import { ROLE_NAMES } from "@/core/roles";
import { CARD_SUITS } from "@/core/types";
import { GameConfig } from "@/core/config";

// types
export type ActionTag = PlayerAction["tag"];
export const RESOURCES = ["Gold", "Cards"] as const;
export type Resource = (typeof RESOURCES)[number];
export type CityDistrictTarget = z.infer<typeof CityDistrictTargetSchema>;
export type BuildMethod = z.infer<typeof BuildMethodSchema>;
export type WizardMethod = z.infer<typeof WizardMethodSchema>;
export type MagicianAction = z.infer<typeof MagicianActionSchema>;
export type Action = Readonly<GameStartAction | PlayerAction>;

export interface GameStartAction extends GameConfig {
  tag: "GameStart";
}

export type PlayerAction = z.infer<typeof PlayerActionSchema>;

export const ActionTagUtils = {
  isResourceGathering(tag: ActionTag): boolean {
    const gatherAction: ActionTag[] = [
      "GatherResourceGold",
      "GatherResourceCards",
      "GatherCardsPick",
    ];
    return gatherAction.includes(tag);
  },

  isRequired(tag: ActionTag): boolean {
    const requiredActions: ActionTag[] = [
      "DraftPick",
      "GatherResourceGold",
      "GatherResourceCards",
      "EndTurn",
    ];
    return requiredActions.includes(tag);
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

const BuildMethodSchema = z.discriminatedUnion("tag", [
  z.object({
    tag: z.literal("Regular"),
    district: DistrictNameSchema,
  }),
  z.object({
    tag: z.literal("Framework"),
    district: DistrictNameSchema,
  }),
  z.object({
    tag: z.literal("Necropolis"),
    sacrifice: CityDistrictTargetSchema,
  }),
  z.object({
    tag: z.literal("ThievesDen"),
    discard: z.array(DistrictNameSchema),
  }),
  z.object({
    tag: z.literal("Cardinal"),
    district: DistrictNameSchema,
    discard: z.array(DistrictNameSchema),
    player: PlayerNameSchema,
  }),
]);

const WizardMethodSchema = z.discriminatedUnion("tag", [
  z.object({
    tag: z.literal("Pick"),
    district: DistrictNameSchema,
  }),
  z.object({
    tag: z.literal("Build"),
    district: DistrictNameSchema,
  }),
  z.object({
    tag: z.literal("Framework"),
    district: DistrictNameSchema,
  }),
  z.object({
    tag: z.literal("Necropolis"),
    sacrifice: CityDistrictTargetSchema,
  }),
  z.object({
    tag: z.literal("ThievesDen"),
    discard: z.array(DistrictNameSchema),
  }),
]);

const MagicianActionSchema = z.discriminatedUnion("tag", [
  z.object({
    tag: z.literal("trade"),
    player: PlayerNameSchema,
  }),
  z.object({
    tag: z.literal("discard"),
    districts: z.array(DistrictNameSchema),
  }),
]);

const DraftDiscardPlayerActionSchema = z.object({
  tag: z.literal("DraftDiscard"),
  role: RoleNameSchema,
});
const GatherResourceGoldPlayerActionSchema = z.object({
  tag: z.literal("GatherResourceGold"),
});
const GatherResourceCardsPlayerActionSchema = z.object({
  tag: z.literal("GatherResourceCards"),
});
const BuildPlayerActionSchema = z.object({
  tag: z.literal("Build"),
  build: BuildMethodSchema,
});
const EndTurnPlayerActionSchema = z.object({
  tag: z.literal("EndTurn"),
});
const GoldFromNobilityPlayerActionSchema = z.object({
  tag: z.literal("GoldFromNobility"),
});
const GoldFromReligionPlayerActionSchema = z.object({
  tag: z.literal("GoldFromReligion"),
});
const GoldFromTradePlayerActionSchema = z.object({
  tag: z.literal("GoldFromTrade"),
});
const GoldFromMilitaryPlayerActionSchema = z.object({
  tag: z.literal("GoldFromMilitary"),
});
const CardsFromNobilityPlayerActionSchema = z.object({
  tag: z.literal("CardsFromNobility"),
});
const MerchantGainOneGoldPlayerActionSchema = z.object({
  tag: z.literal("MerchantGainOneGold"),
});
const ArchitectGainCardsPlayerActionSchema = z.object({
  tag: z.literal("ArchitectGainCards"),
});
const TakeCrownPlayerActionSchema = z.object({
  tag: z.literal("TakeCrown"),
});
const AssassinatePlayerActionSchema = z.object({
  tag: z.literal("Assassinate"),
  role: RoleNameSchema,
});
const MagicPlayerActionSchema = z.object({
  tag: z.literal("Magic"),
  magic: MagicianActionSchema,
});
const BeautifyPlayerActionSchema = z.object({
  tag: z.literal("Beautify"),
  district: CityDistrictTargetSchema,
});
const ScholarRevealPlayerActionSchema = z.object({
  tag: z.literal("ScholarReveal"),
});
const ScholarPickPlayerActionSchema = z.object({
  tag: z.literal("ScholarPick"),
  district: DistrictNameSchema,
});
const EmperorGiveCrownPlayerActionSchema = z.object({
  tag: z.literal("EmperorGiveCrown"),
  player: PlayerNameSchema,
  resource: ResourceSchema,
});
const EmperorHeirGiveCrownPlayerActionSchema = z.object({
  tag: z.literal("EmperorHeirGiveCrown"),
  player: PlayerNameSchema,
});
const TakeFromRichPlayerActionSchema = z.object({
  tag: z.literal("TakeFromRich"),
  player: PlayerNameSchema,
});
const CardsFromReligionPlayerActionSchema = z.object({
  tag: z.literal("CardsFromReligion"),
});
const SendWarrantsPlayerActionSchema = z.object({
  tag: z.literal("SendWarrants"),
  signed: RoleNameSchema,
  unsigned: z.tuple([RoleNameSchema, RoleNameSchema]),
});

export const PlayerActionSchema = z.discriminatedUnion("tag", [
  // draft actions
  z.object({
    tag: z.literal("DraftPick"),
    role: RoleNameSchema,
  }),

  z.object({
    tag: z.literal("DraftDiscard"),
    role: RoleNameSchema,
  }),
  // core actions
  z.object({
    tag: z.literal("GatherCardsPick"),
    district: DistrictNameSchema,
  }),
  // role actions

  // rank 1
  z.object({
    tag: z.literal("MagistrateWarrantResponse"),
    reveal: z.boolean(),
  }),
  // rank 2
  z.object({
    tag: z.literal("Steal"),
    role: RoleNameSchema,
  }),
  z.object({
    tag: z.literal("Bewitch"),
    role: RoleNameSchema,
  }),

  z.object({
    tag: z.literal("Spy"),
    player: PlayerNameSchema,
    suit: CardSuitSchema,
  }),
  z.object({
    tag: z.literal("SpyAcknowledge"),
  }),
  z.object({
    tag: z.literal("Blackmail"),
    flowered: RoleNameSchema,
    unmarked: RoleNameSchema,
  }),
  z.object({
    tag: z.literal("BlackmailerResponse"),
    reveal: z.boolean(),
  }),
  z.object({
    tag: z.literal("BlackmailedResponse"),
    payBribe: z.boolean(),
  }),
  // rank 3
  z.object({
    tag: z.literal("WizardPeek"),
    player: PlayerNameSchema,
  }),
  z.object({
    tag: z.literal("WizardPick"),
    wizard: WizardMethodSchema,
  }),
  // rank 4
  z.object({
    tag: z.literal("PatricianTakeCrown"),
  }),
  // rank 5
  z.object({
    tag: z.literal("CardinalExchange"),
  }),

  z.object({
    tag: z.literal("AbbotGainFromReligion"),
    gold: z.number(),
    cards: z.number(),
  }),
  z.object({
    tag: z.literal("AbbotTakeFromRich"),
    gold: z.number(),
    cards: z.number(),
  }),
  // rank 7
  z.object({
    tag: z.literal("SeerDistribute"),
    seer: z.array(z.tuple([PlayerNameSchema, DistrictNameSchema])),
  }),

  z.object({
    tag: z.literal("NavigatorGain"),
    resource: ResourceSchema,
  }),
  // rank 8
  z.object({
    tag: z.literal("WarlordDestroy"),
    district: CityDistrictTargetSchema,
  }),
  z.object({
    tag: z.literal("MarshalSeize"),
    district: CityDistrictTargetSchema,
  }),
  z.object({
    tag: z.literal("DiplomatTrade"),
    ours: CityDistrictTargetSchema,
    theirs: CityDistrictTargetSchema,
  }),

  // rank 9
  z.object({
    tag: z.literal("QueenGainGold"),
  }),
  z.object({
    tag: z.literal("CollectTaxes"),
  }),
  // district actions
  z.object({
    tag: z.literal("TheaterSwap"),
    role: RoleNameSchema,
    player: PlayerNameSchema,
  }),
  z.object({
    tag: z.literal("TheaterPass"),
  }),
  z.object({
    tag: z.literal("Museum"),
    district: DistrictNameSchema,
  }),
  z.object({
    tag: z.literal("Laboratory"),
    district: DistrictNameSchema,
  }),
  z.object({
    tag: z.literal("Smithy"),
  }),
]);
