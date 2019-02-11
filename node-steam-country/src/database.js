/* Just leaving this here in case switching to MongoDB is ever on the table
   again. */

const mongoose = require('mongoose');
const { database } = require('./constants');

exports.connect = mongoose.connect.bind(this,
    `mongodb+srv://${database.user}:${database.password}@${database.url}`);

const idSchema = new mongoose.Schema({
    ts: Number,
    open: Boolean,
});

idSchema.methods.close = () => {
    this.open = false;
    this.ts = Date.now();
};

idSchema.methods.open = () => {
    this.open = true;
    this.ts = Date.now();
};

idSchema.methods.sample = async () => {
    return await this.model.aggregate([{
        $sample: 1,
    }]);
};

const countrySchema = new mongoose.Schema({
    id: String,
    occ: Number,
});

countrySchema.methods.increase = () => {
    this.occ++;
};

exports.models = {
    Id: mongoose.model('Id', idSchema),
    Country: mongoose.model('Country', countrySchema),
};
