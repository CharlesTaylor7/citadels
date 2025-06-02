import { Museum } from "@/core/game";
import { shuffle } from "@/server/game/random";
import { DistrictName } from "@/core/districts";

const ARTIFACTS = [
  "⚱️",
  "🏺",
  "🖼️",
  "🗿",
  "🏛️",
  "⛲",
  "🕰️",
  "🦴",
  "🦾",
  "⚰️",
  "🚀",
  "🦖",
  "🦣",
  "🦤",
  "🦕",
  "💎",
  "🪩",
  "🔱",
  "🧋",
  "👠",
];

function artifactsOnDisplay(museum: Museum): string[] {
  return museum.artifacts.slice(0, museum.cards.length);
}

function tuck(museum: Museum, card: DistrictName): void {
  // Add the card to our collection
  museum.cards.push(card);

  // If we need more artifacts, generate them
  if (museum.cards.length > museum.artifacts.length) {
    // Create a copy of the artifacts array
    const newArtifacts = Array.from(ARTIFACTS);

    // Shuffle the artifacts (Fisher-Yates algorithm)
    shuffle(newArtifacts, () => Math.random());

    // Add the shuffled artifacts to our collection
    museum.artifacts = museum.artifacts.concat(newArtifacts);
  }
}
