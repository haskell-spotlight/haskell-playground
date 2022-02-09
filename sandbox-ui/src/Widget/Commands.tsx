import React, { useEffect, useState } from 'react';
import * as s from './Commands.module.css';
import TreeView, { RenderNode, Tree } from '../react-haskell/Data/Tree/TreeView';
import * as api from '../api';
import SvgIcon from '../icons/SVGIcon';
import arrowDownIcon from '!!raw-loader!../icons/arrow-down.svg';
import arrowRightIcon from '!!raw-loader!../icons/arrow-right.svg';
import termIcon from '!!raw-loader!../icons/term.svg';
import viewIcon from '!!raw-loader!../icons/view.svg';
import checkUnknownIcon from '!!raw-loader!../icons/check-unknown.svg';

type CommandsProps = {
  serverUrl?: string
}

const Commands = (props: CommandsProps) => {
  const [client, setClient] = useState<ReturnType<typeof api.DefaultApiFp>>();
  const [fsTree, setFsTree] = useState<api.Tree>();
  const [activeCommand, setActiveCommand] = useState<string[]>([]);

  useEffect(() => {
    (async () => {
      const client = api.DefaultApiFp();
      setClient(client);
      const tree = await (await (await client.fsTreeGet(['TermFile', 'CheckFile', 'ViewFile']))()).data;
      setFsTree(tree);

      const defaultCommand = tree.subForest.find(c => c.rootLabel.File && c.rootLabel.File.name.match(/^.*\.default\.(term|check|view)\.([^.]*)$/));
      if (defaultCommand) {
        setActiveCommand(['.', defaultCommand.rootLabel.File!.name]);
      }
    })()
  }, []);

  return (
    <div>
      {fsTree && (
        <div className={s.treeContainer}>
          <TreeView
            tree={fsTree}
            depth={0}
            renderNode={renderNode}
          />
        </div>
      )}

    </div>
  )
}

const sortSubForest = (tree: Tree): Tree => {
  const files = tree.subForest.filter(t => t.rootLabel.File)
    .sort((a, b) => a.rootLabel.File!.name.localeCompare(b.rootLabel.File!.name));
  const dirs = tree.subForest.filter(t => t.rootLabel.Dir)
    .sort((a, b) => a.rootLabel.Dir!.name.localeCompare(b.rootLabel.Dir!.name));

  return {
    ...tree,
    subForest: [...dirs, ...files]
  }
}

const renderNode: RenderNode = (tree, depth) => {
  const isRoot = depth === 0;
  const [isCollapsed, setIsCollapsed] = useState(!isRoot);

  let label = '';
  let icon = null;

  if (tree.Dir) {
    label = tree.Dir.name;
    icon = <SvgIcon svg={isCollapsed ? arrowRightIcon : arrowDownIcon} style={{ transform: 'scale(1.5)', fill: 'var(--text-color)' }} />

  } else if (tree.File && (tree.File.kind === 'TermFile')) {
    label = tree.File?.name.replace(/\.term\.([^.]*)$/, '');
    icon = <SvgIcon svg={termIcon} style={{ fill: 'var(--text-color)' }} />

  } else if (tree.File && (tree.File.kind === 'CheckFile')) {
    label = tree.File?.name.replace(/\.check\.([^.]*)$/, '');
    icon = <SvgIcon svg={checkUnknownIcon} style={{ fill: 'var(--text-color)' }} />

  }
  else if (tree.File && tree.File.kind === 'ViewFile') {
    label = tree.File.name.replace(/\.(view)\.([^.]*)$/, '.$2');
    icon = <SvgIcon svg={viewIcon} style={{ fill: 'var(--text-color)' }} />
  }

  label = label.replace(/^(.*)\.default\.([^.]*)$/, '$1.$2');

  const rootLabel = (
    <div
      className={`${s.fsTreeNode} ${tree.Dir ? s.fsTreeNodeDir : s.fsTreeNodeFile}`}
      title={label}
      onClick={() => setIsCollapsed(!isCollapsed)}
    >
      {isRoot ? null : Array.from(Array(depth - 1)).map((_, i) => <div key={i} className={s.fsTreeNodeIndent}></div>)}
      <div className={s.fsTreeNodeIcon}>{icon}</div>
      <div className={s.fsTreeNodeName}>{label}</div>
    </div>
  );

  const getVisibility = (_: Tree) => ({
    tree: true,
    rootLabel: !isRoot,
    subForest: isRoot || !isCollapsed
  });

  const alterTree = sortSubForest;

  const cssClasses = { node: s.treeNode, rootLabel: s.treeRootLabel, subForest: s.treeSubForest };

  const styles = { node: {}, rootLabel: {}, subForest: {} };

  return {
    rootLabel,
    getVisibility,
    alterTree,
    cssClasses,
    styles
  }
}

export default Commands;
