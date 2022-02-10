import '../styles/fonts.css';
import '../styles/normalize.css';
import '../styles/globals.css';
import * as s from './App.module.css';
import React from 'react';
import ContextTree from './ContextTree';
import { BrowserRouter, Route, Routes } from "react-router-dom";

export type AppProps = {
  serverUrl?: string,
};

const AppBody = (props: AppProps) => (<div className={s.app}>
  <div className={s.commands}>
    <ContextTree serverUrl={props.serverUrl} />
  </div>
</div>);

export const App = (props: AppProps) => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/*" element={<AppBody {...props} />} />
      </Routes>
    </BrowserRouter>
  );
}

export default App;
