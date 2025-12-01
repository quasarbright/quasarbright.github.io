/**
 * ExpressionEvaluator
 * Wraps math.js to parse and evaluate mathematical expressions for vector fields
 */

class ExpressionEvaluator {
    /**
     * Create an expression evaluator for a 2D vector field
     * @param {string} xExpression - Mathematical expression for x component (e.g., "-y")
     * @param {string} yExpression - Mathematical expression for y component (e.g., "x")
     */
    constructor(xExpression, yExpression) {
        this.xExpression = xExpression;
        this.yExpression = yExpression;
        
        // Use global math.js instance loaded from CDN
        this.math = window.math;
        
        // Store compiled expressions and validation state
        this.xCompiled = null;
        this.yCompiled = null;
        this.valid = false;
        this.error = null;
        
        // Attempt to compile expressions
        this._compile();
    }
    
    /**
     * Internal method to compile and validate expressions
     * @private
     */
    _compile() {
        try {
            // Parse expressions to check syntax
            const xNode = this.math.parse(this.xExpression);
            const yNode = this.math.parse(this.yExpression);
            
            // Check for undefined variables (only x and y are allowed)
            const xVars = this._getVariables(xNode);
            const yVars = this._getVariables(yNode);
            const allVars = new Set([...xVars, ...yVars]);
            
            // Remove allowed variables
            allVars.delete('x');
            allVars.delete('y');
            
            if (allVars.size > 0) {
                const undefinedVars = Array.from(allVars).join(', ');
                throw new Error(`Undefined variables: ${undefinedVars}. Only x and y are allowed.`);
            }
            
            // Compile expressions for faster evaluation
            this.xCompiled = xNode.compile();
            this.yCompiled = yNode.compile();
            
            this.valid = true;
            this.error = null;
        } catch (err) {
            this.valid = false;
            this.error = err.message;
            this.xCompiled = null;
            this.yCompiled = null;
        }
    }
    
    /**
     * Extract variable names from a parsed expression node
     * @private
     * @param {Object} node - math.js parse tree node
     * @returns {Set<string>} Set of variable names
     */
    _getVariables(node) {
        const variables = new Set();
        
        node.traverse((node, path, parent) => {
            if (node.type === 'SymbolNode') {
                // Check if this is a variable (not a function or constant)
                // Math.js constants like pi, e are also SymbolNodes
                const name = node.name;
                
                // Exclude math.js constants and functions
                if (!this.math[name] && name !== 'true' && name !== 'false' && name !== 'null') {
                    variables.add(name);
                }
            }
        });
        
        return variables;
    }
    
    /**
     * Evaluate the vector field at a given point
     * @param {number} x - x coordinate
     * @param {number} y - y coordinate
     * @returns {{x: number, y: number}|null} Vector at the point, or null if invalid
     */
    evaluate(x, y) {
        if (!this.valid) {
            return null;
        }
        
        try {
            const scope = { x, y };
            const vx = this.xCompiled.evaluate(scope);
            const vy = this.yCompiled.evaluate(scope);
            
            // Check for NaN or Infinity
            if (!isFinite(vx) || !isFinite(vy)) {
                return null;
            }
            
            return { x: vx, y: vy };
        } catch (err) {
            // Evaluation error at this specific point
            return null;
        }
    }
    
    /**
     * Check if the expressions are valid
     * @returns {boolean} True if expressions are valid
     */
    isValid() {
        return this.valid;
    }
    
    /**
     * Get error message if expressions are invalid
     * @returns {string|null} Error message or null if valid
     */
    getError() {
        return this.error;
    }
}

export { ExpressionEvaluator };
