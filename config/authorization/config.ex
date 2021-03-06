alias Acl.Accessibility.Always, as: AlwaysAccessible
alias Acl.GraphSpec.Constraint.Resource, as: ResourceConstraint
alias Acl.GraphSpec, as: GraphSpec
alias Acl.GroupSpec, as: GroupSpec
alias Acl.GroupSpec.GraphCleanup, as: GraphCleanup

defmodule Acl.UserGroups.Config do
  def user_groups do
    # These elements are walked from top to bottom.  Each of them may
    # alter the quads to which the current query applies.  Quads are
    # represented in three sections: current_source_quads,
    # removed_source_quads, new_quads.  The quads may be calculated in
    # many ways.  The useage of a GroupSpec and GraphCleanup are
    # common.
    [
      %GroupSpec{
        name: "rollvolet",
        useage: [:read, :write, :read_for_write],
        access: %AlwaysAccessible{}, # TODO check related account
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/rollvolet",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://purl.org/goodrelations/v1#BusinessEntity",
                        "http://schema.org/ContactPoint",
                        "http://www.w3.org/ns/locn#Address",
                        "http://purl.org/goodrelations/v1#Offering",
                        "http://purl.org/goodrelations/v1#SomeItems",
                        "http://purl.org/goodrelations/v1#UnitPriceSpecification",
                        "http://data.rollvolet.be/vocabularies/stock-management/WarehouseLocation",
                        "http://data.rollvolet.be/vocabularies/stock-management/WarehouseDepartment",
                        "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
                        "http://purl.org/goodrelations/v1#BusinessEntityType",
                        "http://mu.semte.ch/vocabularies/ext/UnitCode",
                        "http://mu.semte.ch/vocabularies/ext/ProductCategory",
                        "http://mu.semte.ch/vocabularies/ext/OrganizationType",
                        "http://schema.org/Country",
                        "http://schema.org/Language"
                      ]
                    } } ] },

      # // CLEANUP
      #
      %GraphCleanup{
        originating_graph: "http://mu.semte.ch/application",
        useage: [:write],
        name: "clean"
      }
    ]
  end
end
