/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstAbstract.h
 *
 * Abstract class definitions for AST nodes
 *
 ***********************************************************************/

#pragma once

#include "AstNode.h"

namespace souffle {

class AstAtom;

/**
 * Literal
 */
class AstLiteral : public AstNode {
public:
    AstLiteral* clone() const override = 0;
};

/**
 * Atom Literal
 */
class AstAtomLiteral : public AstLiteral {
public:
    /** get atom */
    virtual const AstAtom* getAtom() const = 0;

    AstLiteral* clone() const override = 0;
};

/**
 * Argument
 */
class AstArgument : public AstNode {
public:
    /** Create clone */
    AstArgument* clone() const override = 0;
};

}  // end of namespace souffle
