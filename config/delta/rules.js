export default [
  {
    match: {
      // listen to all changes
    },
    callback: {
      url: "http://resource/.mu/delta",
      method: "POST"
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 500,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      predicate: {
        type: "uri",
        value: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      },
      object: {
        type: "uri",
        value: "http://redpencil.data.gift/vocabularies/tasks/Task",
      },
    },
    callback: {
      url: "http://pricelist-export/delta",
      method: "POST"
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 500,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      // listen to all changes
    },
    callback: {
      url: 'http://search/update',
      method: 'POST'
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 500,
      ignoreFromSelf: true
    }
  }
];
