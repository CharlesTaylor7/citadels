import { anonymousProcedure, loggedInProcedure, router } from ".";
import { z } from "zod";
import {
  DISTRICT_NAMES,
  DistrictName,
  DistrictNameUtils,
} from "@/server/game/districts";
import { DistrictEnum } from "@/server/game/districts";
export const assetRouter = router({
  districts: anonymousProcedure.query(() => {
    return DISTRICT_NAMES.map((name, index) => {});
  }),
});
