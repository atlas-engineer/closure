(defpackage :dom
  (:use)
  (:export
   
   ;; methods
   #:has-feature
   #:doctype
   #:implementation
   #:document-element
   #:create-element
   #:create-document-fragment
   #:create-text-node
   #:create-comment
   #:create-cdata-section
   #:create-processing-instruction
   #:create-attribute
   #:create-entity-reference
   #:get-elements-by-tag-name
   #:node-name
   #:node-value
   #:node-type
   #:parent-node 
   #:child-nodes
   #:first-child
   #:last-child
   #:previous-sibling
   #:next-sibling
   #:attributes
   #:owner-document
   #:insert-before
   #:replace-child
   #:remove-child
   #:append-child
   #:has-child-nodes
   #:clone-node
   #:item
   #:length
   #:get-named-item
   #:set-named-item
   #:remove-named-item
   #:data
   #:substring-data
   #:append-data
   #:insert-data
   #:delete-data
   #:replace-data
   #:name
   #:specified
   #:value
   #:tag-name
   #:get-attribute
   #:set-attribute
   #:remove-atttribute
   #:get-attribute-node
   #:set-attribute-node
   #:remove-attribute-node
   #:normalize
   #:split-text
   #:entities
   #:notations
   #:public-id
   #:system-id
   #:notation-name
   #:target
   
   ;; protocol classes
   #:dom-implementation
   #:document-fragment
   #:document
   #:node
   #:node-list
   #:named-node-map
   #:character-data
   #:attr
   #:element
   #:text
   #:comment
   #:cdata-section
   #:document-type
   #:notation
   #:entity
   #:entity-reference
   #:processing-instruction
   ;;
   #:items
   ;;
   #:node-p
   #:document-p
   #:document-fragment-p
   #:character-data-p
   #:attribute-p
   #:element-p
   #:text-node-p
   #:comment-p
   #:cdata-section-p
   #:document-type-p
   #:notation-p
   #:entity-p
   #:entity-reference-p
   #:processing-instruction-p
   #:named-node-map-p
   ))