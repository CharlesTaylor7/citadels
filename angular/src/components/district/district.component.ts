import { Component, Input, input } from '@angular/core';
import { CommonModule } from '@angular/common';

import {
  DistrictName,
  DistrictData,
  DistrictNameUtils,
} from '@/core/districts';

@Component({
  selector: 'app-district',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './district.component.html',
})
export class DistrictComponent {
  @Input() name: string | null = null;
  @Input() selected: boolean = false;
  @Input() selectable: boolean = false;
  @Input() draggable: boolean = false;
  district?: District;

  ngOnChanges(): void {
    // eslint-disable-next-line
    this.district = DistrictFactory.fromDistrictName(this.name as DistrictName);
  }

  get labelStyle() {
    if (!this.district?.pos) return {};
    return {
      'z-index': this.district.pos.z,
      transform: `translate(${this.district.pos.x}px, ${this.district.pos.y}px)`,
    };
  }

  get districtNameMarginClass() {
    if (!this.district?.value) return {};
    return {
      'ml-2': this.district.value === 'SchoolOfMagic',
      'ml-4': this.district.value === 'ImperialTreasury',
      'ml-3': this.district.value === 'HauntedQuarter',
    };
  }

  getSuitBgColorClass(suit: CardSuit): string {
    switch (suit) {
      case 'Military':
        return 'bg-suit-military';
      case 'Religious':
        return 'bg-suit-religious';
      case 'Noble':
        return 'bg-suit-noble';
      case 'Trade':
        return 'bg-suit-trade';
      case 'Unique':
        return 'bg-suit-unique';
      default:
        return '';
    }
  }

  get svgFilterStyle() {
    if (!this.district?.asset) return {};
    return `brightness(${this.district.asset.brightness})`;
  }

  get artifactContainerStyle() {
    if (!this.district?.asset) return {};
    return {
      height: `${this.district.asset.height}px`,
    };
  }
}

export interface DistrictPosition {
  x: number;
  y: number;
  z: number;
}

export interface DistrictAsset {
  height: number | string;
  width: number | string;
  brightness: number;
  offset_x: number;
  offset_y: number;
  scale_percentage: number;
  path: string;
}

export type CardSuit = 'Military' | 'Religious' | 'Noble' | 'Trade' | 'Unique';

export interface District {
  value: string;
  name: string;
  description?: string | null;
  suit: CardSuit;
  cost?: number | null;
  beautified: boolean;
  pos: DistrictPosition;
  asset: DistrictAsset;
  artifacts: string[];
}

class DistrictFactory {
  public static fromDistrictName(
    districtName: DistrictName,
  ): District | undefined {
    const data: DistrictData = DistrictNameUtils.data(districtName);
    if (!data) return undefined;

    const length = 170.0;
    const scale = 10.0;
    const { x: p_x, y: p_y } = this.crop(districtName);
    // 'saturate' is calculated by lighting() but not part of TS DistrictAsset interface, so it's not used in 'asset'
    const { brightness /*, saturate */ } = this.lighting(districtName);

    const crop_offset_x = p_x * length;
    const crop_offset_y = p_y * length;

    const full_height = (length * scale) / 5.0;
    const full_width = length * (125.8 / 200.0) * (scale / 5.0);

    const asset: DistrictAsset = {
      brightness,
      path: '/public/districts.jpeg',
      height: length,
      width: length,
      scale_percentage: scale * 100.0,
      offset_x: -crop_offset_x - full_width * (data.id % 10),
      offset_y: -crop_offset_y - full_height * Math.floor(data.id / 10),
    };

    return {
      value: districtName,
      name: data.displayName,
      cost: districtName === 'SecretVault' ? null : data.cost,
      suit: data.suit,
      description: data.description ?? null,
      beautified: false, // Default value
      pos: { x: 0, y: 0, z: 0 }, // Default position
      artifacts: [], // Default empty artifacts
      asset,
    };
  }
  private static crop(districtName: DistrictName): { x: number; y: number } {
    switch (districtName) {
      // yellow
      case 'Manor':
        return { x: 0.236, y: 0.2 };
      case 'Palace':
        return { x: 0.236, y: 0.05 };

      // blue
      case 'Temple':
        return { x: 0.236, y: 0.3 };
      case 'Church':
        return { x: 0.236, y: 0.4 };
      case 'Monastery':
        return { x: 0.236, y: 0.7 };
      case 'Cathedral':
        return { x: 0.236, y: 0.7 };

      // green
      case 'Market':
        return { x: 0.236, y: 0.7 };
      case 'Tavern':
        return { x: 0.236, y: 0.4 };
      case 'TradingPost':
        return { x: 0.236, y: 0.7 };
      case 'Docks':
        return { x: 0.236, y: 0.5 };
      case 'Harbor':
        return { x: 0.236, y: 0.7 };
      case 'TownHall':
        return { x: 0.236, y: 0.6 };

      // red
      case 'Prison':
        return { x: 0.236, y: 0.3 };
      case 'Barracks':
        return { x: 0.236, y: 0.3 };
      case 'Fortress':
        return { x: 0.236, y: 0.15 };

      case 'Library':
        return { x: 0.27, y: 0.3 };
      case 'GoldMine':
        return { x: 0.236, y: 0.3 };
      case 'Statue':
        return { x: 0.236, y: 0.0 };
      case 'SchoolOfMagic':
        return { x: 0.236, y: 0.3 };
      case 'ImperialTreasury':
        return { x: 0.236, y: 0.3 };
      case 'Observatory':
        return { x: 0.236, y: 0.12 };
      case 'MapRoom':
        return { x: 0.236, y: 0.4 };
      case 'DragonGate':
        return { x: 0.236, y: 0.4 };
      case 'SecretVault':
        return { x: 0.236, y: 0.15 };
      case 'Quarry':
        return { x: 0.236, y: 0.5 };
      case 'HauntedQuarter':
        return { x: 0.236, y: 0.4 };
      case 'GreatWall':
        return { x: 0.236, y: 0.2 };
      case 'WishingWell':
        return { x: 0.236, y: 0.1 };
      case 'Park':
        return { x: 0.25, y: 0.0 };
      case 'Museum':
        return { x: 0.27, y: 0.1 };
      case 'IvoryTower':
        return { x: 0.236, y: 0.05 };
      case 'Laboratory':
        return { x: 0.236, y: 0.5 };
      case 'Theater':
        return { x: 0.236, y: 0.2 };
      case 'PoorHouse':
        return { x: 0.236, y: 0.5 };
      case 'Smithy':
        return { x: 0.236, y: 0.2 };
      case 'Framework':
        return { x: 0.236, y: 0.2 };
      case 'ThievesDen':
        return { x: 0.236, y: 0.3 };
      case 'Basilica':
        return { x: 0.236, y: 0.1 };
      case 'Monument':
        return { x: 0.236, y: 0.1 };
      case 'Factory':
        return { x: 0.236, y: 0.1 };
      case 'Capitol':
        return { x: 0.236, y: 0.1 };
      default:
        return { x: 0.236, y: 0.0 };
    }
  }

  private static lighting(districtName: DistrictName): {
    brightness: number;
    saturate: number;
  } {
    switch (districtName) {
      // yellow
      case 'Manor':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Palace':
        return { brightness: 1.3, saturate: 1.0 };

      // blue
      case 'Temple':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Church':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Monastery':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Cathedral':
        return { brightness: 1.5, saturate: 1.0 };

      // green
      case 'Market':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Tavern':
        return { brightness: 1.3, saturate: 1.0 };
      case 'TradingPost':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Docks':
        return { brightness: 1.5, saturate: 1.0 };
      case 'Harbor':
        return { brightness: 1.3, saturate: 1.0 };
      case 'TownHall':
        return { brightness: 1.3, saturate: 1.0 };

      // red
      case 'Prison':
        return { brightness: 1.5, saturate: 1.0 };
      case 'Barracks':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Fortress':
        return { brightness: 1.5, saturate: 1.0 };

      case 'Library':
        return { brightness: 1.0, saturate: 1.0 };
      case 'GoldMine':
        return { brightness: 1.5, saturate: 1.0 };
      case 'Statue':
        return { brightness: 1.3, saturate: 1.0 };
      case 'SchoolOfMagic':
        return { brightness: 1.5, saturate: 1.0 };
      case 'ImperialTreasury':
        return { brightness: 1.5, saturate: 1.0 };
      case 'Observatory':
        return { brightness: 2.0, saturate: 1.0 };
      case 'MapRoom':
        return { brightness: 1.5, saturate: 1.0 };
      case 'DragonGate':
        return { brightness: 1.5, saturate: 1.0 };
      case 'SecretVault':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Quarry':
        return { brightness: 1.3, saturate: 1.0 };
      case 'HauntedQuarter':
        return { brightness: 1.3, saturate: 1.0 };
      case 'GreatWall':
        return { brightness: 1.3, saturate: 1.0 };
      case 'WishingWell':
        return { brightness: 2.0, saturate: 2.0 };
      case 'Park':
        return { brightness: 1.2, saturate: 1.0 };
      case 'Museum':
        return { brightness: 1.2, saturate: 1.0 };
      case 'IvoryTower':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Laboratory':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Theater':
        return { brightness: 1.3, saturate: 1.0 };
      case 'PoorHouse':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Smithy':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Framework':
        return { brightness: 1.3, saturate: 1.0 };
      case 'ThievesDen':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Basilica':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Monument':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Factory':
        return { brightness: 1.3, saturate: 1.0 };
      case 'Capitol':
        return { brightness: 1.3, saturate: 1.0 };
      default:
        return { brightness: 1.3, saturate: 1.0 };
    }
  }
}
