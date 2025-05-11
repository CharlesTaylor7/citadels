import { CardSet, CardSuit } from "@/core/types";
import { ActionTag } from "@/core/actions";
import { ROLES } from "@/core/characters";

/**
 * All available ranks in the game
 */
export const RANKS = [
  "One",
  "Two",
  "Three",
  "Four",
  "Five",
  "Six",
  "Seven",
  "Eight",
  "Nine",
] as const;

/**
 * Type representing a rank in the game
 */
export type Rank = (typeof RANKS)[number];

/**
 * Utility functions for Rank
 */
export const RankUtils = {
  /**
   * Get all ranks as an array
   * @returns Array of all ranks
   */
  all(): Rank[] {
    return [...RANKS];
  },

  /**
   * Get the next rank after the given rank
   * @param rank The current rank
   * @returns The next rank, or undefined if there is no next rank
   */
  next(rank: Rank): Rank | undefined {
    const index = RANKS.indexOf(rank);
    return index < RANKS.length - 1 ? RANKS[index + 1] : undefined;
  },

  /**
   * Convert a rank to its numeric index
   * @param rank The rank to convert
   * @returns The numeric index of the rank
   */
  toIndex(rank: Rank): number {
    return RANKS.indexOf(rank);
  },

  /**
   * Convert a rank to its string representation
   * @param rank The rank to convert
   * @returns String representation of the rank (1-9)
   */
  toString(rank: Rank): string {
    return (RANKS.indexOf(rank) + 1).toString();
  },
};

/**
 * All available role names in the game
 * Laid out in the order of the asset file for their images
 */
export const ROLE_NAMES = [
  "Assassin",
  "Witch",
  "Magistrate",

  "Thief",
  "Spy",
  "Blackmailer",

  "Magician",
  "Wizard",
  "Seer",

  "King",
  "Emperor",
  "Patrician",

  "Bishop",
  "Abbot",
  "Cardinal",

  "Merchant",
  "Alchemist",
  "Trader",

  "Architect",
  "Navigator",
  "Scholar",

  "Warlord",
  "Diplomat",
  "Marshal",

  "Queen",
  "Artist",
  "TaxCollector",
] as const;

/**
 * Type representing a role name in the game
 */
export type RoleName = (typeof ROLE_NAMES)[number];

/**
 * Utility functions for RoleName
 */
export const RoleNameUtils = {
  /**
   * Get all role names as an array
   * @returns Array of all role names
   */
  all(): RoleName[] {
    return [...ROLE_NAMES];
  },

  /**
   * Get the rank of a role
   * @param role The role to get the rank for
   * @returns The rank of the role
   */
  rank(role: RoleName): Rank {
    return this.data(role).rank;
  },

  /**
   * Get the display name of a role
   * @param role The role to get the display name for
   * @returns The display name of the role
   */
  displayName(role: RoleName): string {
    if (role === "TaxCollector") {
      return "Tax Collector";
    }
    return role;
  },

  /**
   * Get the minimum player count required for a role
   * @param role The role to check
   * @returns The minimum player count required for the role
   */
  minPlayerCount(role: RoleName): number {
    switch (role) {
      case "Queen":
        return 5;
      case "Emperor":
      case "Artist":
      case "TaxCollector":
        return 3;
      default:
        return 0;
    }
  },

  /**
   * Get the data for a role
   * @param role The role to get data for
   * @returns The role data
   */
  data(role: RoleName): RoleData {
    return ROLES[ROLE_NAMES.indexOf(role)];
  },

  /**
   * Check if a role can be discarded face up
   * @param role The role to check
   * @returns True if the role can be discarded face up, false otherwise
   */
  canBeDiscardedFaceUp(role: RoleName): boolean {
    // rank 4 cards cannot be discarded faceup during the draft.
    // see rulebook page 3
    return this.data(role).rank !== "Four";
  },

  /**
   * Get the build limit for a role
   * @param role The role to check
   * @returns The number of districts the role can build
   */
  buildLimit(role: RoleName): number {
    switch (role) {
      case "Architect":
        return 3;
      case "Navigator":
        return 0;
      case "Scholar":
      case "Seer":
        return 2;
      default:
        return 1;
    }
  },
};
/**
 * Immutable data for a role
 */
export interface RoleData {
  name: RoleName;
  rank: Rank;
  set: CardSet;
  suit: CardSuit | null;
  description: string;
  reminder: string;
  actions: Array<[number, ActionTag]>;
}
