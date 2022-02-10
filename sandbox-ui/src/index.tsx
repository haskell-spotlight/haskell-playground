import React from 'react';
import { render as reactDOMRender } from 'react-dom';
import _App, { AppProps } from './App/App';

export const App = _App;

export function render(target: HTMLElement, props: AppProps) {
  reactDOMRender(<App {...props} />, target);
}
