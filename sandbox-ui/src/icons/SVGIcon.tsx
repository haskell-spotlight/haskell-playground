import React, { CSSProperties } from 'react';
import * as s from './SVGIcon.module.css';

export type SvgIconProps = {
  svg: string,
  style?: CSSProperties
}

const SvgIcon = (props: SvgIconProps) => {
  return (<div className={s.svgIcon} style={props.style || {}} dangerouslySetInnerHTML={{ __html: props.svg }}></div>);
}

export default SvgIcon;
