import React, { Component } from 'react'

import { Markdown } from './utils'
import Modal from 'react-modal'

const modalStyles = {
  content : {
    top                   : '50%',
    left                  : '50%',
    right                 : 'auto',
    bottom                : 'auto',
    marginRight           : '-50%',
    transform             : 'translate(-50%, -50%)'
  }
}

export default class PersonModal extends Component {
  constructor(props) {
    super(props)

    this.person = this.props.person
    this.state = { isOpen: false }
  }

  componentDidMount = () => {
    this.setState({ isOpen: this.props.isOpen })
  }

  removePerson = () => this.props.removePerson(this.person)
  handleClose = () => this.props.handleClose(this.person)

  render() {    
    return (
      <Modal
        ariaHideApp={false}
        className="col-sm-12 col-md-3"
        style={modalStyles}
        isOpen={this.props.isOpen}
        onRequestClose={this.handleClose}
      >

        <div className="card">
          <div className="card-body">
            <h3>{this.person.name}</h3>

            <div className="row">
              <div className="col-md-6 col-12"><b>Phone: </b>{this.person.phone}</div>
              <div className="col-md-6 col-12"><b>Email: </b>{this.person.email}</div>
              <div className="col-md-6 col-12"><b>Born: </b>{this.person.birth}</div>            
            </div>

            <hr />

            <Markdown text={this.person.notes} />
          </div>

          <div className="card-footer">
            <button
              className="btn btn-outline-danger"
              onClick={this.removePerson}
            >
              Remove
            </button>
          </div>
        </div>
      </Modal>
    )
  }
}