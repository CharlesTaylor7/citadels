import { RoleName } from "@/core/roles";
import { DistrictName } from "@/core/districts";

export const CONFIG_OPTIONS = ["Sometimes", "Always", "Never"] as const;
export type ConfigOption = (typeof CONFIG_OPTIONS)[number];

export interface GameOptions {
  roleAnarchy: boolean;
  roles: Set<RoleName>;
  districts: Map<DistrictName, ConfigOption>;
}

export interface PlayerConfig {
  id: number;
  name: string;
}
export interface GameConfig {
  players: PlayerConfig[];
  config: GameOptions;
  rngSeed: Seed;
}

type Seed = number;
