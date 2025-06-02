import { Component, input } from '@angular/core';
import { CommonModule } from '@angular/common';
import { DistrictComponent } from '../district/district.component';
import { DistrictName } from '@/core/districts';

@Component({
  selector: 'app-hand-panel',
  standalone: true,
  imports: [CommonModule, DistrictComponent],
  templateUrl: './hand-panel.component.html',
})
export class HandPanelComponent {
  districts = input<DistrictName[]>();
}
