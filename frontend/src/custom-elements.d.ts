import React from "react";
import { DistrictName } from "./districts";

declare module "react" {
  namespace JSX {
    interface IntrinsicElements {
      ["citadels-district"]: {
        name: DistrictName;
        draggable?: boolean;
        selectable?: boolean;
      };
    }
  }
}
