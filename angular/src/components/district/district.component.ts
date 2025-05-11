import { Component, Input } from '@angular/core';
import { CommonModule } from '@angular/common';

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

@Component({
  selector: 'app-district',
  standalone: true,
  imports: [CommonModule], // Import CommonModule for Angular directives
  templateUrl: './district.component.html',
})
export class DistrictComponent {
  @Input() district!: District;
  @Input() enabled: boolean = true;
  @Input() draggable: boolean = false;
  @Input() inputType: string = 'checkbox';

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
