import React from 'react';
import * as api from '../../../api';

// Just a start of an experiment to implement UI for of Haskell types.
// Specific case of https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Tree.html#g:1
// Swagger generated types don't fit here. Makes more sense to use direct conversion aeson -> TS types.
// Probably this project: https://github.com/codedownio/aeson-typescript#readme
export type HaskellTree = api.Tree;
export type HaskellTreeNode = api.Node;

export type TreeProps = {
  tree: HaskellTree,
  renderNode: RenderNode,
  cssClasses: {
    node: string,
    rootLabel: string,
    subForest: string
  },
}

export type RenderNode = (props: HaskellTreeNode) => React.ReactNode;

const Tree = (props: TreeProps) => {
  return (
    <div>
      <div>{props.renderNode(props.tree.rootLabel)}</div>
      {props.tree.subForest.length > 0 && (
        <div>
          {props.tree.subForest.map(tree => <Tree key={JSON.stringify(tree.rootLabel)} {...props} tree={tree} />)}
        </div>
      )}
    </div>
  );
}

export default Tree;
