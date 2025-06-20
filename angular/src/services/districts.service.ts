import districts from '../../../public/districts.json';
import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root',
})
export class DistrictService {
  private districtMap = new Map(districts.map((d) => [d.name, d]));

  getDistrictData(districtName: DistrictName): DistrictData {
    // @ts-expect-error It will work
    return this.districtMap.get(districtName);
  }
}

// TODO: fix these
export type DistrictName = string;
export type CardSuit = 'Military' | 'Religious' | 'Noble' | 'Trade' | 'Unique';

export interface DistrictData {
  id: number;
  name: DistrictName;
  display_name: string;
  cost?: number;
  suit: CardSuit;
  set: string;
  description?: string;
  multiplicity: number;
}
