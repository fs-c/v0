import React, { Component } from 'react'

import marked from 'marked'

export class Markdown extends Component {
  render() {
    const md = marked(this.props.text || '')

    return (<div>
      <div dangerouslySetInnerHTML={{ __html: md }} />
    </div>)
  }
}
