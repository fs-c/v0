import { Component } from '@angular/core';
import { FormBuilder } from '@angular/forms';
import * as TreeModel from 'tree-model';

class Node {
  name: String;
  parent: Node | null;
  children: Node[];

  constructor(name: String, parent: Node | null = null) {
    this.name = name;
    this.parent = parent;
    this.children = [];
  }
}

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  nodeForm;

  root;
  tree: TreeModel;

  constructor(private formBuilder: FormBuilder) {
    this.tree = new TreeModel();

    this.nodeForm = this.formBuilder.group({
      name: '',
      parent: '',
    });
  }

  onSubmit(rawNode) {
    console.log('node submitted', rawNode);

    if (!this.root) {
      console.log('ignoring node parent, assigning to root');

      this.root = this.tree.parse({ name: rawNode.name, children: [] });
    } else {
      const parent = this.root.first((node) =>
        node.model.name === rawNode.parent);
      
      if (!parent) {
        console.error('given parent does not exist');
      } else {
        const node = this.tree.parse({ name: rawNode.name, children: [] });

        parent.addChild(node);
      }
    }

    this.root.walk((node) => {
      console.log({ node, index: node.index, name: node.model.name }); 
    });

    this.nodeForm.reset();
  }
}
