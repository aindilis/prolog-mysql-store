-- ============================================================================
-- MySQL Assertion Store Schema
-- ============================================================================

-- Contexts: Isolation boundaries for knowledge bases
CREATE TABLE contexts (
    context_id INT PRIMARY KEY AUTO_INCREMENT,
    context_name VARCHAR(100) UNIQUE NOT NULL,
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    INDEX idx_name (context_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Formulae: Core fact storage
CREATE TABLE formulae (
    formula_id BIGINT PRIMARY KEY AUTO_INCREMENT,
    context_id INT NOT NULL,
    functor VARCHAR(255) NOT NULL,
    arity TINYINT UNSIGNED NOT NULL,
    
    -- Serialized representations
    term_canonical TEXT NOT NULL,      -- write_canonical for reconstruction
    term_readable TEXT,                -- pretty print for debugging
    term_hash CHAR(64),                -- SHA256 for deduplication
    
    -- Metadata
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    INDEX idx_context_functor (context_id, functor, arity),
    INDEX idx_functor_arity (functor, arity),
    INDEX idx_hash (term_hash),
    FOREIGN KEY (context_id) REFERENCES contexts(context_id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Indexed arguments: Fast lookup on simple arguments
CREATE TABLE arguments_indexed (
    formula_id BIGINT NOT NULL,
    arg_position TINYINT UNSIGNED NOT NULL,
    arg_type ENUM('atom', 'integer', 'float', 'string') NOT NULL,
    
    -- Type-specific storage
    atom_value VARCHAR(255),
    int_value BIGINT,
    float_value DOUBLE,
    string_value VARCHAR(255),
    
    PRIMARY KEY (formula_id, arg_position),
    INDEX idx_atom (atom_value),
    INDEX idx_int (int_value),
    INDEX idx_float (float_value),
    INDEX idx_string (string_value),
    FOREIGN KEY (formula_id) REFERENCES formulae(formula_id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- List elements: Optional table for querying list contents
CREATE TABLE list_elements (
    formula_id BIGINT NOT NULL,
    arg_position TINYINT UNSIGNED NOT NULL,
    element_position SMALLINT UNSIGNED NOT NULL,
    element_type ENUM('atom', 'integer', 'float', 'compound') NOT NULL,
    element_value VARCHAR(255),
    
    INDEX idx_element (element_value),
    INDEX idx_formula_arg (formula_id, arg_position),
    FOREIGN KEY (formula_id) REFERENCES formulae(formula_id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Metadata: Extensible key-value storage
CREATE TABLE metadata (
    formula_id BIGINT NOT NULL,
    meta_key VARCHAR(100) NOT NULL,
    meta_value TEXT,
    
    PRIMARY KEY (formula_id, meta_key),
    INDEX idx_key (meta_key),
    FOREIGN KEY (formula_id) REFERENCES formulae(formula_id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Statistics: Track usage for optimization
CREATE TABLE predicate_stats (
    context_id INT NOT NULL,
    functor VARCHAR(255) NOT NULL,
    arity TINYINT UNSIGNED NOT NULL,
    
    query_count BIGINT DEFAULT 0,
    assert_count BIGINT DEFAULT 0,
    retract_count BIGINT DEFAULT 0,
    last_accessed TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    PRIMARY KEY (context_id, functor, arity),
    INDEX idx_access (last_accessed),
    FOREIGN KEY (context_id) REFERENCES contexts(context_id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Predicate cache status
CREATE TABLE cache_status (
    context_id INT NOT NULL,
    functor VARCHAR(255) NOT NULL,
    arity TINYINT UNSIGNED NOT NULL,
    
    is_loaded BOOLEAN DEFAULT FALSE,
    load_time TIMESTAMP NULL,
    fact_count INT DEFAULT 0,
    
    PRIMARY KEY (context_id, functor, arity),
    FOREIGN KEY (context_id) REFERENCES contexts(context_id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
