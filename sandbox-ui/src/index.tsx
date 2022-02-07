import React from 'react';
import { render as reactDOMRender } from 'react-dom';
import _Widget, { WidgetProps } from './Widget/Widget';

export const Widget = _Widget;

export function render(target: HTMLElement, props: WidgetProps) {
  reactDOMRender(<Widget {...props} />, target);
}
