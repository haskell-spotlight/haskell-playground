import React, { AnchorHTMLAttributes, forwardRef, ForwardedRef, useContext } from 'react';

export type WidgetProps = {
  sandboxUrl: string
};

export const Widget = (props: WidgetProps) => {
  return (
    <div>{props.sandboxUrl}</div>
  );
}

export default Widget;
