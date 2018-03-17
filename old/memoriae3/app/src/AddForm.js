import React, { Component } from 'react'

class TextInput extends Component {
  constructor(props) {
    super(props)

    this.state = { value: '' }
  }

  handleChange = e => {
    this.props.handleChange(e)
    this.setState({ value: e.target.value })

    e.target.style.height = 'auto'
    e.target.style.height = e.target.scrollHeight + 'px'
  }

  render() {
    return (
      <div className="form-group">
        <label>{this.props.name} <small className="text-muted">{this.props.small}</small></label>
        {this.props.type !== 'textarea' ? (
          <input
            name={this.props.shortName || this.props.name.toLowerCase()}
            value={this.state.value}
            onChange={this.handleChange}
            type="text"
            className="form-control" />
        ) : (
          <textarea
            name={this.props.shortName || this.props.name.toLowerCase()}
            value={this.state.value}
            onChange={this.handleChange}
            style={{ overflowY: 'hidden' }}
            type="textarea"
            className="form-control" />
        )}
      </div>
    )
  }
}

export default class AddForm extends Component {
  constructor() {
    super()

    this.state = { }
  }

  add = e => {
    e.preventDefault()

    this.props.handleAdd(this.state)
  }

  handleChange = e => this.setState({ [e.target.name]: e.target.value })

  render() {
    return (
      <form id="form-add-person" className="form">
        <TextInput 
          name="Name"
          handleChange={this.handleChange} />

        <TextInput 
          name="Phone"
          handleChange={this.handleChange} />
        
        <TextInput 
          name="Email"
          handleChange={this.handleChange} />
        
        <TextInput 
          name="Date of birth"
          small="(MM/DD/YYYY)"
          shortName="birth"
          handleChange={this.handleChange} />
        
        <TextInput 
          name="Notes"
          small="Also supports markdown!"
          type="textarea"
          handleChange={this.handleChange} />

        <button type="submit" className="btn btn-primary btn-block" onClick={this.add}>
          Add
        </button>
      </form>
    )
  }
}