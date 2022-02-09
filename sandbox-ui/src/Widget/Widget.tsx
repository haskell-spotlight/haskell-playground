import '../styles/fonts.css';
import '../styles/normalize.css';
import '../styles/globals.css';
import * as s from './Widget.module.css';
import React from 'react';
import Commands from './Commands';

export type WidgetProps = {
  serverUrl?: string,
};

export const Widget = (props: WidgetProps) => {
  return (
    <div className={s.widget}>
      <div className={s.commands}>
        <Commands serverUrl={props.serverUrl} />
      </div>
    </div>
  );
}

export default Widget;
