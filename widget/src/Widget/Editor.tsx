import React, { useEffect, useRef, useState, RefObject } from "react";
import * as monaco from 'monaco-editor';
import * as s from './Editor.module.css';
import { registerHaskellSupport } from './monaco-haskell';

type EditorProps = {
  code: string,
  onChange: (code: string) => void
}

const Editor = (props: EditorProps) => {
  const containerRef = useRef<HTMLDivElement>(null);
  const [editor, setEditor] = useState<monaco.editor.IStandaloneCodeEditor | undefined>();

  useEffect(() => {
    registerHaskellSupport(monaco);
  }, []);

  useEffect(() => {
    if (containerRef.current && !editor) {
      const editor = monaco.editor.create(
        containerRef.current,
        {
          value: props.code,
          language: 'haskell',
          minimap: { enabled: false },
          theme: 'vs-dark'
        });
      setEditor(editor);
    }
  }, [containerRef.current]);

  return (
    <div ref={containerRef} className={s.container}></div>
  );
}

export default Editor;
