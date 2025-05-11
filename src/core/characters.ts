import { RoleData } from "@/core/roles";

export const ROLES: RoleData[] = [
  // Rank 1 characters
  {
    rank: "One",
    name: "Assassin",
    set: "Base",
    suit: null,
    actions: [[1, "Assassinate"]],
    description:
      "Call a character you wish to kill. The killed character skips their turn.",
    reminder: "",
  },
  {
    rank: "One",
    name: "Witch",
    set: "DarkCity",
    suit: null,
    actions: [[1, "Bewitch"]],
    description:
      "Gather resources, call a character you wish to bewitch, then put your turn on hold. After the bewitched character gathers resources, you resume your turn as that character.",
    reminder:
      "Gathering resources becomes a forced first step of your turn and of the bewitched player's turn. You gain both most active and passive abilities from the target, i.e. architect increased build limit, and bishop's protection from warlord. But you still rely on your own hand, own city and own gold. If you bewitch the King or Patrician, then they still get the crown. But you can use their other ability. You can bewitch the Emperor as normal.",
  },
  {
    rank: "One",
    name: "Magistrate",
    set: "Citadels2016",
    suit: null,
    actions: [[1, "SendWarrants"]],
    description:
      "Assign warrants to character cards. Reveal the signed warrant to confiscate the first district that player builds. The player gets back all gold paid to build that district.",
    reminder:
      "There are 3 warrants, 1 signed and 2 unsigned. Revealing the signed warrant is optional.",
  },

  // Rank 2 characters
  {
    rank: "Two",
    name: "Thief",
    set: "Base",
    suit: null,
    actions: [[1, "Steal"]],
    description:
      "Call a character you wish to rob. When the robbed character is revealed you take all their gold.",
    reminder: "",
  },
  {
    rank: "Two",
    name: "Spy",
    set: "Citadels2016",
    suit: null,
    actions: [[1, "Spy"]],
    description:
      "Name a district type and look at another player's hand. For each card of that type, take 1 of their gold and gain 1 card.",
    reminder: "You gain cards from the deck, not their hand.",
  },
  {
    rank: "Two",
    name: "Blackmailer",
    set: "Citadels2016",
    suit: null,
    actions: [[1, "Blackmail"]],
    description:
      "Assign threats facedown to character cards. A threatened player can bribe you (half their gold rounded down) to remove their threat. If you reveal the flower, you take all their gold.",
    reminder:
      "There are two threat markers, 1 flowered, and 1 normal. Revealing the normal marker has no effect. A threatened player must gather resources first on their turn and then decide whether to bribe the blackmailer. If not bribed, the blackmailer then decides if they want to reveal the marker. The cost of bribery is 0, if the player has only 1 gold.",
  },

  // Rank 3 characters
  {
    rank: "Three",
    name: "Magician",
    set: "Base",
    suit: null,
    actions: [[1, "Magic"]],
    description:
      "Either exchange hands of cards with another player or discard any number of cards to gain an equal number of cards.",
    reminder: "",
  },
  {
    rank: "Three",
    name: "Wizard",
    set: "DarkCity",
    suit: null,
    actions: [[1, "WizardPeek"]],
    description:
      "Look at another player's hand and choose 1 card. Either pay to build it immediately or add it to your hand. You can build identical districts.",
    reminder: "",
  },
  {
    rank: "Three",
    name: "Seer",
    set: "Citadels2016",
    suit: null,
    actions: [[1, "SeerTake"]],
    description:
      "Randomly take 1 card from each player's hand and add it to yours. Then give each player you took a card from 1 card from your hand. You can build up to 2 districts.",
    reminder: "If someone's hand was empty, you don't give them a card.",
  },

  // Rank 4 characters
  {
    rank: "Four",
    name: "King",
    set: "Base",
    suit: "Noble",
    actions: [
      [1, "TakeCrown"],
      [1, "GoldFromNobility"],
    ],
    description:
      "Take the crown. Gain 1 gold for each of your NOBLE districts.",
    reminder: "",
  },
  {
    rank: "Four",
    name: "Emperor",
    set: "DarkCity",
    suit: "Noble",
    actions: [
      [1, "EmperorGiveCrown"],
      [1, "GoldFromNobility"],
    ],
    description:
      "Give the crown to another player and take 1 of their gold or 1 of their cards. Gain 1 gold for each of your NOBLE districts.",
    reminder: "Only available in 3+ player games.",
  },
  {
    rank: "Four",
    name: "Patrician",
    set: "Citadels2016",
    suit: "Noble",
    actions: [
      [1, "PatricianTakeCrown"],
      [1, "GoldFromNobility"],
    ],
    description:
      "Take the crown. At the end of the game, score 1 extra point for each NOBLE district in your city. Gain 1 gold for each of your NOBLE districts.",
    reminder: "",
  },

  // Rank 5 characters
  {
    rank: "Five",
    name: "Bishop",
    set: "Base",
    suit: "Religious",
    actions: [[1, "GoldFromReligion"]],
    description:
      "The rank 8 character cannot use its ability on your districts. Gain 1 gold for each of your RELIGIOUS districts.",
    reminder: "",
  },
  {
    rank: "Five",
    name: "Abbot",
    set: "DarkCity",
    suit: "Religious",
    actions: [
      [1, "TakeFromRich"],
      [1, "ResourcesFromReligion"],
    ],
    description:
      "Take 1 gold or 1 card from the player with the most gold. Gain 1 gold or 1 card for each of your RELIGIOUS districts.",
    reminder: "",
  },
  {
    rank: "Five",
    name: "Cardinal",
    set: "Citadels2016",
    suit: "Religious",
    actions: [
      [1, "CardinalExchange"],
      [1, "GoldFromReligion"],
    ],
    description:
      "When another player builds a district, you can exchange cards from your hand for their gold. Gain 1 gold for each of your RELIGIOUS districts.",
    reminder: "",
  },

  // Rank 6 characters
  {
    rank: "Six",
    name: "Merchant",
    set: "Base",
    suit: "Trade",
    actions: [
      [1, "MerchantGainOneGold"],
      [1, "GoldFromTrade"],
    ],
    description:
      "Gain 1 extra gold. Gain 1 gold for each of your TRADE districts.",
    reminder: "",
  },
  {
    rank: "Six",
    name: "Alchemist",
    set: "DarkCity",
    suit: null,
    actions: [],
    description:
      "At the end of your turn, you get back all the gold you paid to build districts this turn. You cannot pay more gold than you have.",
    reminder:
      "You don't get back gold spent for other reasons, e.g. tax collector",
  },
  {
    rank: "Six",
    name: "Trader",
    set: "Citadels2016",
    suit: "Trade",
    actions: [[1, "GoldFromTrade"]],
    description:
      "You can build any number of TRADE districts. Gain 1 gold for each of your TRADE districts.",
    reminder: "",
  },

  // Rank 7 characters
  {
    rank: "Seven",
    name: "Architect",
    set: "Base",
    suit: null,
    actions: [[1, "ArchitectGainCards"]],
    description: "Gain 2 extra cards. You can build up to 3 districts.",
    reminder: "",
  },
  {
    rank: "Seven",
    name: "Navigator",
    set: "DarkCity",
    suit: null,
    actions: [[1, "NavigatorGain"]],
    description:
      "Gain either 4 extra gold or 4 extra cards. You cannot build any districts.",
    reminder: "",
  },
  {
    rank: "Seven",
    name: "Scholar",
    set: "Citadels2016",
    suit: null,
    actions: [[1, "ScholarReveal"]],
    description:
      "Draw 7 cards, choose 1 to keep, then shuffle the rest back into the deck. You can build up to 2 districts.",
    reminder: "",
  },

  // Rank 8 characters
  {
    rank: "Eight",
    name: "Warlord",
    set: "Base",
    suit: "Military",
    actions: [
      [1, "GoldFromMilitary"],
      [1, "WarlordDestroy"],
    ],
    description:
      "Destroy 1 district by paying 1 fewer gold than its cost. Gain 1 gold for each of your MILITARY districts.",
    reminder:
      "You cannot target a completed city. You cannot target the Bishop's city.",
  },
  {
    rank: "Eight",
    name: "Diplomat",
    set: "DarkCity",
    suit: "Military",
    actions: [
      [1, "GoldFromMilitary"],
      [1, "DiplomatTrade"],
    ],
    description:
      "Exchange 1 of your districts for another player's district, giving them gold equal to the difference in their costs. Gain 1 gold for each of your MILITARY districts.",
    reminder: "",
  },
  {
    rank: "Eight",
    name: "Marshal",
    set: "Citadels2016",
    suit: "Military",
    actions: [
      [1, "GoldFromMilitary"],
      [1, "MarshalSeize"],
    ],
    description:
      "Seize 1 district with a cost of 3 or less from another player's city, giving that player gold equal to its cost. Gain 1 gold for each of your MILITARY districts.",
    reminder: "",
  },

  // Rank 9 characters
  {
    rank: "Nine",
    name: "Queen",
    set: "Citadels2016",
    suit: null,
    actions: [[1, "QueenGainGold"]],
    description:
      "If you are sitting next to the rank 4 character, gain 3 gold.",
    reminder: "Only available in 5+ player games.",
  },
  {
    rank: "Nine",
    name: "Artist",
    set: "DarkCity",
    suit: null,
    actions: [[2, "Beautify"]],
    description:
      "Beautify up to 2 of your districts by assigning each of them 1 of your gold. A district can be beautified only once.",
    reminder:
      "A beautified district is worth 1 more point and its cost is raised by 1 gold. For example, the Warlord has to pay 1 more to destroy a beautified city.",
  },
  {
    rank: "Nine",
    name: "TaxCollector",
    set: "Citadels2016",
    suit: null,
    actions: [[1, "CollectTaxes"]],
    description:
      "After each player builds, they place 1 of their gold on the Tax Collector's character card. Take all gold from character card.",
    reminder:
      "The Tax Collector is not taxed. Taxes are always collected, even if the Tax Collector was killed, or discarded during the draft. In 2-3 player games, you are taxed when playing as your non-Tax Collector character.",
  },
];
