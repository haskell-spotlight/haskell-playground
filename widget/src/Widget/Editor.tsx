import React, { useEffect, useRef, useState } from 'react';
import * as s from './Editor.module.css';

export type EditorProps = {
  sandboxUrl: string
}

const Editor = (props: EditorProps) => {
  const [_, forceUpdate] = useState();

  useEffect(() => {
    forceUpdate(undefined);
  }, []);

  return (
    <div className={s.container}>
      <iframe
        className={s.terminal}
        src={`http://${props.sandboxUrl}/editor`}
        allow='autoplay'
        frameBorder="0"
      />
    </div>
  );
}

export default Editor;
