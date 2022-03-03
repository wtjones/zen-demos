#pragma once

#include "../array_node.hpp"
#include "../evaluator_types.hpp"
#include "../integer_node.hpp"
#include "../nil_node.hpp"
#include "../string_node.hpp"
#include "loitar/core/node_visitor.hpp"

#include <string>
#include <vector>

namespace loitar::syslib {

/**
 * Implement aref with vistor pattern.
 *
 * Pseudocode:
 *  ar.evaluate(params)
 *       this.params = params;
 *       params[0].accept(this)
 *           visit(ArrayNode n):
 *               for rest of params;
 *                   visit(IntegerNode i_node):
 *                       // append to int vector
 *               if 0 == this.messages.len():
 *                   this.result = n.get_element(int vec)
 *           visit(StrinNode n):
 *               params[1].accept(this):
 *                   visit(IntegerNode i_node):
 *                       // append to int vector
 *               if 0 == this.messages.len():
 *                   auto c = n.value()
 *                           .at(this.str_index));
 *                   this.result = new AtomNode(c);
 */
class ArefEvaluator : public NodeVisitor {
public:
    ArefEvaluator();
    virtual ~ArefEvaluator();
    void visit(ArrayNode& node);
    void visit(IntegerNode& node);
    void visit(StringNode& node);
    EvaluatorNodeResult evaluate(std::vector<std::shared_ptr<Node>> params);

private:
    std::vector<size_t> m_indexes;
    EvaluatorNodeResult m_result;
    std::vector<std::shared_ptr<Node>> m_params;
};
}
