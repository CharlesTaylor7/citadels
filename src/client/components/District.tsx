import React from "react";
import { type DistrictName } from "@/server/game/districts";
import { type CardSuit } from "@/server/game/types";

interface DistrictAsset {
  height: number;
  width: number;
  brightness: number;
  offset_x: number;
  offset_y: number;
  scale_percentage: number;
  path: string;
}

interface DistrictProps {
  value: DistrictName;
  name: string;
  description?: string;
  suit: CardSuit;
  cost?: number;
  beautified?: boolean;
  pos?: {
    x: number;
    y: number;
    z: number;
  };
  asset: DistrictAsset;
  artifacts: string[];
  enabled: boolean;
  draggable: boolean;
}
// Helper function to determine suit background color
const getSuitBgColor = (suit: CardSuit): string => {
  switch (suit) {
    case "Noble":
      return "bg-yellow-300";
    case "Trade":
      return "bg-green-300";
    case "Military":
      return "bg-red-300";
    case "Religious":
      return "bg-blue-300";
    case "Unique":
      return "bg-purple-300";
    default:
      return "bg-gray-300";
  }
};

export function District({
  district,
  enabled = true,
  draggable = false,
}: DistrictProps) {
  return (
    <label
      data-dropzone=""
      data-district={district.value}
      id={`card-${district.value}`}
      className={`
        relative
        select-none
        cursor-pointer
        p-1 box-content
        rounded-2xl
        focus-visible:ring-4
        has-[:checked]:p-0
        has-[:checked]:border
        has-[:checked]:border-4
        has-[:checked]:border-green-500
        ${enabled ? "cursor-pointer" : ""}
        ${draggable ? "draggable" : ""}
      `}
      data-draggable={draggable ? "true" : undefined}
      data-x={`${district.pos.x}px`}
      data-y={`${district.pos.y}px`}
      style={{
        zIndex: district.pos.z,
        transform: `translate(${district.pos.x}px, ${district.pos.y}px)`,
      }}
    >
      <div
        data-tip={district.description}
        className={`
          w-full
          rounded-t-xl relative h-10 bg-neutral-700
          ${district.description ? "tooltip tooltip-bottom tooltip-secondary before:w-full" : ""}
        `}
      >
        <div
          className={`
            absolute top-2
            leaded
            w-full text-center 
            font-serif
            text-base text-neutral-100
            ${district.value === "SchoolOfMagic" ? "ml-2" : ""}
            ${district.value === "ImperialTreasury" ? "ml-4" : ""}
            ${district.value === "HauntedQuarter" ? "ml-3" : ""}
          `}
        >
          {district.name}
        </div>
        <code
          className={`
            absolute top-2 left-2
            h-6 w-6 rounded-full
            leaded 
            flex flex-row justify-center items-center 
            text-black
            ${getSuitBgColor(district.suit)}
          `}
        >
          {district.cost}
        </code>
        {district.beautified && (
          <div
            data-tip="Beautified"
            className="
              absolute right-2 top-1 
              tooltip tooltip-secondary 
              text-2xl 
            "
          >
            ✨
          </div>
        )}
      </div>
      <svg
        className="rounded-b-xl"
        xmlns="http://www.w3.org/2000/svg"
        xmlnsXlink="http://www.w3.org/1999/xlink"
        height={district.asset.height}
        width={district.asset.width}
        style={{ filter: `brightness(${district.asset.brightness})` }}
      >
        <image
          id="img"
          x={district.asset.offset_x}
          y={district.asset.offset_y}
          height={`${district.asset.scale_percentage}%`}
          xlinkHref={district.asset.path}
          preserveAspectRatio="xMinYMin slice"
        />
      </svg>
      <div
        className="absolute top-10"
        style={{ height: `${district.asset.height}px` }}
      >
        <div className="flex flex-row flex-wrap gap-0 justify-center items-center h-full">
          {district.artifacts.map((item, index) => (
            <div
              key={index}
              className="rounded-full leaded h-10 w-10 draggable text-4xl bg-gradient-radial from-indigo-500 text-center"
            >
              {item}
            </div>
          ))}
        </div>
      </div>
    </label>
  );
}
