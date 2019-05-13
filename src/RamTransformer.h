/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamTransformer.h
 *
 * Defines the interface for RAM transformation passes.
 *
 ***********************************************************************/

#pragma once

#include <cassert>
#include <functional>
#include <memory>
#include <string>
#include <vector>
#include <iostream>

namespace souffle {

class RamTranslationUnit;

/**
 * Abstract Transformer Class for RAM
 */

class RamTransformer {
public:
    RamTransformer() = default;
    virtual ~RamTransformer() = default;

    /** Apply transformer */
    bool apply(RamTranslationUnit& translationUnit);

    /** Get name of transformer */
    virtual std::string getName() const = 0;

protected:
    /** Transform RAM translation unit */
    virtual bool transform(RamTranslationUnit& translationUnit) = 0;
};

/**
 * Composite sequence transformer; a series of transformation is applied
 * sequentially. The last transformation decides the outcome whether 
 * the code has been changed. 
 */

class RamTransformerSequence : public RamTransformer {
public:
    template <typename... Tfs>
    RamTransformerSequence(std::unique_ptr<Tfs>&&... tf) : RamTransformerSequence() {
        std::unique_ptr<RamTransformer> tmp[] = {std::move(tf)...};
        for (auto& cur : tmp) {
            transformers.emplace_back(std::move(cur));
        }
        for (const auto& cur : transformers) {
            (void)cur;
            assert(cur);
        }
    }
    RamTransformerSequence() = default;

    std::string getName() const {
        return "RamTransformerSequence";
    }

    bool transform(RamTranslationUnit& tU) {
        bool changed = false;

        // last transformer decides change flag
        for (auto const& cur : transformers) {
            changed = cur->apply(tU);
        }
        return changed;
    }

protected:
    std::vector<std::unique_ptr<RamTransformer>> transformers;
};

/**
 * Composite loop transfomer; iterate until no change
 */

class RamLoopTransformer : public RamTransformer {
public:
    RamLoopTransformer(std::unique_ptr<RamTransformer> tLoop) : loop(std::move(tLoop)) {}

    std::string getName() const {
        return "RamLoopTransformer";
    }

    bool transform(RamTranslationUnit& tU) {
        while (loop->apply(tU))
		;
        return false;
    }

protected:
    std::unique_ptr<RamTransformer> loop;
};

/**
 * Composite conditional transfomer; checks the condition and depending on the condition the transformer is
 * executed or not.
 */

class RamConditionalTransformer : public RamTransformer {
public:
    RamConditionalTransformer(std::function<bool()> fn, std::unique_ptr<RamTransformer> tb)
            : func(fn), body(std::move(tb)) {}

    std::string getName() const {
        return "RamConditionalTransformer";
    }

    bool transform(RamTranslationUnit& tU) {
        if (func()) {
            return body->apply(tU);
        } else {
            return false;
        }
    }

protected:
    std::function<bool()> func;
    std::unique_ptr<RamTransformer> body;
};

}  // end of namespace souffle
