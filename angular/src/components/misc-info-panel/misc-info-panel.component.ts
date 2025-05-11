import { Component, input } from '@angular/core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-misc-info-panel',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './misc-info-panel.component.html',
})
export class MiscInfoPanelComponent {
  miscInfo = input<any>(); // Corresponds to misc.* in Rust template
}
