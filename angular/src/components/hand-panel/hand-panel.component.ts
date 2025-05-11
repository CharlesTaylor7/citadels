import { Component, input } from '@angular/core';
import { CommonModule } from '@angular/common';
import { DistrictComponent } from '../district/district.component'; // To display individual districts

@Component({
  selector: 'app-hand-panel',
  standalone: true,
  imports: [CommonModule, DistrictComponent],
  templateUrl: './hand-panel.component.html',
})
export class HandPanelComponent {
  myPlayer = input<any>(); // Corresponds to my.hand in Rust template
}
