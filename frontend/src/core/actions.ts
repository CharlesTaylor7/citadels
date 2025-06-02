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
  z.object({
    tag: z.literal("GatherResourceGold"),
  }),
  z.object({
    tag: z.literal("GatherResourceCards"),
  }),
  z.object({
    tag: z.literal("Build"),
    build: BuildMethodSchema,
  }),
  z.object({
    tag: z.literal("EndTurn"),
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
    tag: z.literal("SeerTake"),
    player: PlayerNameSchema,
  }),
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
    tag: z.literal("GoldFromNobility"),
  }),

  z.object({
    tag: z.literal("CardsFromNobility"),
  }),
  z.object({
    tag: z.literal("PatricianTakeCrown"),
  }),
  // rank 5

  z.object({
    tag: z.literal("CardsFromReligion"),
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
    tag: z.literal("GoldFromMilitary"),
  }),
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

  z.object({
    tag: z.literal("Armory"),
    district: CityDistrictTargetSchema,
  }),
  z.object({
    tag: z.literal("GoldFromReligion"),
  }),
  z.object({
    tag: z.literal("GoldFromTrade"),
  }),
  z.object({
    tag: z.literal("MerchantGainOneGold"),
  }),
  z.object({
    tag: z.literal("ArchitectGainCards"),
  }),
  z.object({
    tag: z.literal("TakeCrown"),
  }),
  z.object({
    tag: z.literal("Assassinate"),
    role: RoleNameSchema,
  }),
  z.object({
    tag: z.literal("Magic"),
    magic: MagicianActionSchema,
  }),
  z.object({
    tag: z.literal("Beautify"),
    district: CityDistrictTargetSchema,
  }),
  z.object({
    tag: z.literal("ScholarReveal"),
  }),
  z.object({
    tag: z.literal("ScholarPick"),
    district: DistrictNameSchema,
  }),
  z.object({
    tag: z.literal("EmperorGiveCrown"),
    player: PlayerNameSchema,
    resource: ResourceSchema,
  }),
  z.object({
    tag: z.literal("EmperorHeirGiveCrown"),
    player: PlayerNameSchema,
  }),
  z.object({
    tag: z.literal("SendWarrants"),
    signed: RoleNameSchema,
    unsigned: z.tuple([RoleNameSchema, RoleNameSchema]),
  }),
]);
