import { Component, input } from '@angular/core';
import { CommonModule } from '@angular/common';
import { DistrictComponent } from '../district/district.component'; // To display individual districts

@Component({
  selector: 'app-player-city-panel',
  standalone: true,
  imports: [CommonModule, DistrictComponent],
  templateUrl: './player-city-panel.component.html',
})
export class PlayerCityPanelComponent {
  cityData = input<any>(); // Corresponds to city.* in Rust template
}
