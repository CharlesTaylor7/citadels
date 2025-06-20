import { bootstrapApplication } from '@angular/platform-browser';
import { SandboxComponent } from './components/sandbox/sandbox.component';
import { provideExperimentalZonelessChangeDetection } from '@angular/core';

bootstrapApplication(SandboxComponent, {
  providers: [provideExperimentalZonelessChangeDetection()],
});
