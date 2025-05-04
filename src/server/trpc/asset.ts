import { anonymousProcedure, loggedInProcedure, router } from ".";
import { z } from "zod";
import { DISTRICT_NAMES, DistrictNameUtils } from "@/server/game/districts";
import { DistrictEnum } from "@/server/game/districts";

function transformDistrictData(district) {}

function cropDistrict(name) {
  switch (name) {
    case "Manor":
      return { x: 0.236, y: 0.2 };
    case "Palace":
      return { x: 0.236, y: 0.05 };
    case "Temple":
      return { x: 0.236, y: 0.3 };
    case "Church":
      return { x: 0.236, y: 0.4 };
    case "Monastery":
      return { x: 0.236, y: 0.7 };
    case "Cathedral":
      return { x: 0.236, y: 0.7 };
    case "Market":
      return { x: 0.236, y: 0.7 };
    case "Tavern":
      return { x: 0.236, y: 0.4 };
    case "TradingPost":
      return { x: 0.236, y: 0.7 };
    case "Docks":
      return { x: 0.236, y: 0.5 };
    case "Harbor":
      return { x: 0.236, y: 0.7 };
    case "TownHall":
      return { x: 0.236, y: 0.6 };
    case "Prison":
      return { x: 0.236, y: 0.3 };
    case "Baracks":
      return { x: 0.236, y: 0.3 };
    case "Fortress":
      return { x: 0.236, y: 0.15 };
    case "Library":
      return { x: 0.27, y: 0.3 };
    case "GoldMine":
      return { x: 0.236, y: 0.3 };
    case "Statue":
      return { x: 0.236, y: 0.0 };
    case "SchoolOfMagic":
      return { x: 0.236, y: 0.3 };
    case "ImperialTreasury":
      return { x: 0.236, y: 0.3 };
    case "Observatory":
      return { x: 0.236, y: 0.12 };
    case "MapRoom":
      return { x: 0.236, y: 0.4 };
    case "DragonGate":
      return { x: 0.236, y: 0.4 };
    case "SecretVault":
      return { x: 0.236, y: 0.15 };
    case "Quarry":
      return { x: 0.236, y: 0.5 };
    case "HauntedQuarter":
      return { x: 0.236, y: 0.4 };
    case "GreatWall":
      return { x: 0.236, y: 0.2 };
    case "WishingWell":
      return { x: 0.236, y: 0.1 };
    case "Park":
      return { x: 0.25, y: 0.0 };
    case "Museum":
      return { x: 0.27, y: 0.1 };
    case "IvoryTower":
      return { x: 0.236, y: 0.05 };
    case "Laboratory":
      return { x: 0.236, y: 0.5 };
    case "Theater":
      return { x: 0.236, y: 0.2 };
    case "PoorHouse":
      return { x: 0.236, y: 0.5 };
    case "Smithy":
      return { x: 0.236, y: 0.2 };
    case "Framework":
      return { x: 0.236, y: 0.2 };
    case "ThievesDen":
      return { x: 0.236, y: 0.3 };
    case "Basilica":
      return { x: 0.236, y: 0.1 };
    case "Monument":
      return { x: 0.236, y: 0.1 };
    case "Factory":
      return { x: 0.236, y: 0.1 };
    case "Capitol":
      return { x: 0.236, y: 0.1 };
    default:
      return { x: 0.236, y: 0.0 };
  }
}

function lightingDistrict(name) {
  switch (name) {
    case "Manor":
      return { brightness: 1.3, saturate: 1.0 };
    case "Palace":
      return { brightness: 1.3, saturate: 1.0 };
    case "Temple":
      return { brightness: 1.3, saturate: 1.0 };
    case "Church":
      return { brightness: 1.3, saturate: 1.0 };
    case "Monastery":
      return { brightness: 1.3, saturate: 1.0 };
    case "Cathedral":
      return { brightness: 1.5, saturate: 1.0 };
    case "Market":
      return { brightness: 1.3, saturate: 1.0 };
    case "Tavern":
      return { brightness: 1.3, saturate: 1.0 };
    case "TradingPost":
      return { brightness: 1.3, saturate: 1.0 };
    case "Docks":
      return { brightness: 1.5, saturate: 1.0 };
    case "Harbor":
      return { brightness: 1.3, saturate: 1.0 };
    case "TownHall":
      return { brightness: 1.3, saturate: 1.0 };
    case "Prison":
      return { brightness: 1.5, saturate: 1.0 };
    case "Baracks":
      return { brightness: 1.3, saturate: 1.0 };
    case "Fortress":
      return { brightness: 1.5, saturate: 1.0 };
    case "Library":
      return { brightness: 1.0, saturate: 1.0 };
    case "GoldMine":
      return { brightness: 1.5, saturate: 1.0 };
    case "Statue":
      return { brightness: 1.3, saturate: 1.0 };
    case "SchoolOfMagic":
      return { brightness: 1.5, saturate: 1.0 };
    case "ImperialTreasury":
      return { brightness: 1.5, saturate: 1.0 };
    case "Observatory":
      return { brightness: 2.0, saturate: 1.0 };
    case "MapRoom":
      return { brightness: 1.5, saturate: 1.0 };
    case "DragonGate":
      return { brightness: 1.5, saturate: 1.0 };
    case "SecretVault":
      return { brightness: 1.3, saturate: 1.0 };
    case "Quarry":
      return { brightness: 1.3, saturate: 1.0 };
    case "HauntedQuarter":
      return { brightness: 1.3, saturate: 1.0 };
    case "GreatWall":
      return { brightness: 1.3, saturate: 1.0 };
    case "WishingWell":
      return { brightness: 2.0, saturate: 2.0 };
    case "Park":
      return { brightness: 1.2, saturate: 1.0 };
    case "Museum":
      return { brightness: 1.2, saturate: 1.0 };
    case "IvoryTower":
      return { brightness: 1.3, saturate: 1.0 };
    case "Laboratory":
      return { brightness: 1.3, saturate: 1.0 };
    case "Theater":
      return { brightness: 1.3, saturate: 1.0 };
    case "PoorHouse":
      return { brightness: 1.3, saturate: 1.0 };
    case "Smithy":
      return { brightness: 1.3, saturate: 1.0 };
    case "Framework":
      return { brightness: 1.3, saturate: 1.0 };
    case "ThievesDen":
      return { brightness: 1.3, saturate: 1.0 };
    case "Basilica":
      return { brightness: 1.3, saturate: 1.0 };
    case "Monument":
      return { brightness: 1.3, saturate: 1.0 };
    case "Factory":
      return { brightness: 1.3, saturate: 1.0 };
    case "Capitol":
      return { brightness: 1.3, saturate: 1.0 };
    default:
      return { brightness: 1.3, saturate: 1.0 };
  }
}

export const assetRouter = router({
  districts: anonymousProcedure.query(() => {
    return DISTRICT_NAMES.map((name, index) => {
      const length = 170.0;
      const scale = 10.0;
      const crop = cropDistrict(name);
      const lighting = lightingDistrict(name);
      const offset_x = crop.x * length;
      const offset_y = crop.y * length;

      const full_height = (length * scale) / 5.0;
      const full_width = length * (125.8 / 200.0) * (scale / 5.0);

      return {
        ...DistrictNameUtils.data(name),
        asset: {
          saturate: lighting.saturate,
          brightness: lighting.brightness,
          path: "/public/districts.jpeg",
          height: length,
          width: length,
          scale_percentage: scale * 100.0,
          offset_x: -offset_x + -full_width * (index % 10),
          offset_y: -offset_y + -full_height * (index / 10),
        },
      };
    });
  }),
});
