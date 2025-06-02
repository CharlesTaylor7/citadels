import type { DistrictName, DistrictData } from "@/core/districts";
import { type CardSuit } from "@/core/types";
import { DistrictNameUtils } from "@/core/districts";

interface Props {
  name: DistrictName;
  beautified?: boolean;
  pos?: {
    x: number;
    y: number;
    z: number;
  };
  artifacts?: string[];
  enabled?: boolean;
  height: number;
  square?: boolean;
}

export function District(props: Props) {
  const data = DistrictNameUtils.data(props.name);
  const assetPath = `/public/districts/${String(data.id).padStart(2, "0")}.webp`;
  const { brightness, saturate } = districtLighting(props.name);

  const lightingStyle = {
    filter: `brightness(${brightness}) saturate(${props.enabled ? saturate : 0}`,
  };

  function viewbox() {
    if (props.square) return undefined;

    const s = props.height;
    const { x, y } = districtCrop(props.name);
    const tx = x * s * 0.5;
    const ty = y * s * 0.5;
    return `${tx} ${ty} ${s} ${s}`;
  }
  return (
    <div
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
        ${props.enabled ? "cursor-pointer" : ""}
      `}
      style={{
        zIndex: props.pos?.z,
        transform: props.pos
          ? `translate(${props.pos.x}px, ${props.pos.y}px)`
          : undefined,
      }}
    >
      <svg
        className="rounded-b-xl"
        xmlns="http://www.w3.org/2000/svg"
        xmlnsXlink="http://www.w3.org/1999/xlink"
        style={lightingStyle}
        viewBox={viewbox()}
      >
        <image
          id="img"
          xlinkHref={assetPath}
          height="100%"
          preserveAspectRatio="xMinYMin slice"
        />
      </svg>
      <div className="absolute top-10" style={{ height: `${props.height}px` }}>
        <div className="flex flex-row flex-wrap gap-0 justify-center items-center h-full">
          {props.artifacts &&
            props.artifacts.map((item, index) => (
              <div
                key={index}
                className="rounded-full leaded h-10 w-10 draggable text-4xl bg-gradient-radial from-indigo-500 text-center"
              >
                {item}
              </div>
            ))}
        </div>
      </div>
    </div>
  );
}

interface DistrictHeaderProps {
  data: DistrictData;
  beautified?: boolean;
}
function DistrictHeader({ data, beautified }: DistrictHeaderProps) {
  return (
    <div
      data-tip={data.description}
      className={`
          w-full
          rounded-t-xl relative h-10 bg-neutral-700
          ${data.description ? "tooltip tooltip-bottom tooltip-secondary before:w-full" : ""}
        `}
    >
      <div
        className={`
            absolute top-2
            leaded
            w-full text-center 
            font-serif
            text-base text-neutral-100
            ${data.name === "SchoolOfMagic" ? "ml-2" : ""}
            ${data.name === "ImperialTreasury" ? "ml-4" : ""}
            ${data.name === "HauntedQuarter" ? "ml-3" : ""}
          `}
      >
        {data.displayName}
      </div>
      <code
        className={`
            absolute top-2 left-2
            h-6 w-6 rounded-full
            leaded 
            flex flex-row justify-center items-center 
            text-black
            ${getSuitBgColor(data.suit)}
          `}
      >
        {data.cost}
      </code>
      {beautified && (
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
  );
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

function districtCrop(name: DistrictName) {
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
    case "Barracks":
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

function districtLighting(name: DistrictName) {
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
    case "Barracks":
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
