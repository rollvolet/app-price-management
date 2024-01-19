alias Acl.Accessibility.Always, as: AlwaysAccessible
alias Acl.Accessibility.ByQuery, as: AccessByQuery
alias Acl.GraphSpec.Constraint.Resource, as: ResourceConstraint
alias Acl.GraphSpec.Constraint.ResourceFormat, as: ResourceFormatConstraint
alias Acl.GraphSpec, as: GraphSpec
alias Acl.GroupSpec, as: GroupSpec
alias Acl.GroupSpec.GraphCleanup, as: GraphCleanup

defmodule Acl.UserGroups.Config do

  defp authenticated_access() do
    %AccessByQuery{
      vars: [],
      query: "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      SELECT ?account WHERE {
          <SESSION_ID> session:account ?account .
      } LIMIT 1"
    }
  end

  def user_groups do
    # These elements are walked from top to bottom.  Each of them may
    # alter the quads to which the current query applies.  Quads are
    # represented in three sections: current_source_quads,
    # removed_source_quads, new_quads.  The quads may be calculated in
    # many ways.  The useage of a GroupSpec and GraphCleanup are
    # common.
    [
      %GroupSpec{
        name: "public",
        useage: [:read, :read_for_write],
        access: %AlwaysAccessible{},
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/public",
            constraint: %ResourceConstraint{
              resource_types: [

              ]
            }
          }
        ]
      },

      %GroupSpec{
        name: "rollvolet-write",
        useage: [:read, :write, :read_for_write],
        access: authenticated_access(),
        graphs: [
          %GraphSpec{
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
            }
          }
        ]
      },

      %GroupSpec{
        name: "rollvolet-read",
        useage: [:read, :read_for_write],
        access: authenticated_access(),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/users",
            constraint: %ResourceConstraint{
              resource_types: [
                "http://xmlns.com/foaf/0.1/Person", # users
                "http://xmlns.com/foaf/0.1/OnlineAccount"
              ]
            }
          },
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/sessions",
            constraint: %ResourceFormatConstraint{
              resource_prefix: "http://mu.semte.ch/sessions/"
            }
          }
        ]
      },

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
