import React, { ReactNode } from "react";
import * as s from './Tabs.module.css';

export type Tab = {
  id: string,
  title: string,
};

export type TabsProps = {
  tabs: Tab[],
  activeTab: string,
  onTabChange: (tabId: string) => void
}

const Tabs = (props: TabsProps) => {
  return (
    <div className={s.tabs}>
      <div className={s.tabPicker}>
        {props.tabs.map(tab => {
          const isActive = tab.id === props.activeTab;
          return (
            <div key={tab.id} className={s.tabPickerItemContainer}>
              <div
                className={`${s.tabPickerItem} ${isActive ? s.tabPickerItemActive : ''}`}
                onClick={() => props.onTabChange(tab.id)}
              >
                {tab.title}
              </div>
            </div >
          );
        })}
      </div >
    </div >
  );
}

export default Tabs;
