#ifndef TYPE_H_
#define TYPE_H_
#include <memory>
#include <string>
#include <bitset>
#include <vector>
#include <cinttypes>
#include "SymbolTable.h"

class Token;

class Type;
class BuiltInType;
class PointerType;
class ArrayType;
class CompoundType;
class IncompleteType;
class UserDefinedTypeRef;
class FuncType;
class AliasType;
class EnumType;


class Type
{
public:
	enum Tag
	{
        Incomplete,
		BuiltIn, Array, Pointer,
		/* will be in symbol table: */
		Alias /* typedef */, Enum, Compound /* I mean struct and union */,
        UserDefined,
		Func
	};

	enum Qualifier
	{
		Const = 1, Volatile = 2, /** volatile is not implemented */
	};

    using QualifierHolder = uint8_t;

	explicit Type(Tag, QualifierHolder);
	virtual ~Type() {};

	// non-virtual.
	Tag tag() const;
	bool isConst() const;
    void isConst(bool b);
/*	bool isStatic() const;
	bool isExternal() const;*/

    void width(size_t w) { width_ = w; } // TODO : some more reasonable way
    size_t width() const { return width_; }

    virtual std::string toString() const = 0; // for test

    virtual bool equalUnqual(const std::shared_ptr<Type>& t) = 0;
    bool equal(const std::shared_ptr<Type>& t);

    virtual std::shared_ptr<Type> shallowCopy() const {
        assert(false);
    }

    static bool isInteger(const std::shared_ptr<Type>& ty);
	static bool isFloating(const std::shared_ptr<Type>& ty);
    static bool isVoid(const std::shared_ptr<Type>& ty);
    static bool isArithmetic(const std::shared_ptr<Type>& ty);
    static bool isPointer(const std::shared_ptr<Type>& ty);
    static bool isArray(const std::shared_ptr<Type>& ty);
    static bool isScalar(const std::shared_ptr<Type>& ty);
    static std::shared_ptr<PointerType> arrayDecay(const std::shared_ptr<Type>& ty);
    static std::shared_ptr<Type> derefIfIsUserDefinedType(const std::shared_ptr<Type>& ty);
    static size_t alignAt(const std::shared_ptr<Type>& ty);
protected:
	const Tag tag_;
	QualifierHolder qualifiers_;
	size_t width_;
};

class BuiltInType : public Type
{
public:
    enum BuiltInSpecifier
    {
        BI_Unsigned = 32, BI_Short = 64, BI_Long = 128, /* prefix */
        BI_Int = 1, BI_Char = 2, BI_Double = 4, BI_Float = 8, BI_VOID = 16
    };

    enum Category
    {
        Void = 0, Integer, Floating,
    };

    using SpecifierHolder = uint8_t;

    BuiltInType(Category, bool isUnsigned, size_t width, QualifierHolder);
    static std::shared_ptr<BuiltInType> make(SpecifierHolder, QualifierHolder);

    Category cat() const { return cat_; }
    std::string toString() const override;
    std::shared_ptr<BuiltInType> builtinClone() const;
    std::shared_ptr<Type> shallowCopy() const override {
        return builtinClone();
    }

    bool equalUnqual(const std::shared_ptr<Type>& t) override;

    bool isInteger() const {
        return cat_ == Integer;
    }

    bool isFloating() const {
        return cat_ == Floating;
    }

    bool isUnsigned() const {
        assert(isInteger());
        return isUnsigned_;
    }

    void isUnsigned(bool u) {
        assert(isInteger());
        isUnsigned_ = u;
    }

    static std::shared_ptr<BuiltInType> maxUIntType();
    static std::shared_ptr<BuiltInType> intType();
    static std::shared_ptr<BuiltInType> charType();
    static std::shared_ptr<BuiltInType> doubleType();
    static std::shared_ptr<BuiltInType> voidType();
private:
    Category cat_;
    bool isUnsigned_;
};

class PointerType : public Type
{
public:
	PointerType(const std::shared_ptr<Type>&, QualifierHolder);
	std::shared_ptr<Type> base() const;

    std::string toString() const override;

    bool equalUnqual(const std::shared_ptr<Type>& t) override;

    std::shared_ptr<Type> shallowCopy() const override {
        return std::make_shared<PointerType>(base_, qualifiers_);
    }

    static std::shared_ptr<PointerType> strLiteralType();
private:
	std::shared_ptr<Type> base_;
};

class ArrayType : public Type
{
public:
    ArrayType(const std::shared_ptr<Type>&, size_t len,
              QualifierHolder = 0);
    explicit ArrayType(size_t len, QualifierHolder = 0);
    std::shared_ptr<Type> base() const;
    void base(std::shared_ptr<Type> ty);
    size_t len() const;

    std::string toString() const override;

    bool equalUnqual(const std::shared_ptr<Type>& rhs) override;
    std::shared_ptr<Type> shallowCopy() const override {
        return std::make_shared<ArrayType>(base_, len_, qualifiers_);
    }
private:
	std::shared_ptr<Type> base_;
	size_t len_;
};

// user-defined types:
// the following three type
// CompoundType, AliasType and EnumType are only used for meta item in symbol table
// Normal variables refer to the meta item
class CompoundType : public Type
{
public:
    enum MemModel { Compound_Struct, Compound_Union };
    using value_type = std::pair<std::string, std::shared_ptr<Type>>; // (name, type) pair
    CompoundType(std::string, MemModel, SymbolTable* , ListSymtab&&);
    MemModel model() const { return model_; }

    ListSymtab::iterator begin();
    ListSymtab::iterator end();

    ListSymtab& members() { return members_; }

    std::string toString() const override;
	std::string name() const { return name_; }
    SymbolTable* whereDefined() { return whereDefined_; }

    bool equalUnqual(const std::shared_ptr<Type>& t) override;
    size_t alignAt() const { return alignAt_; }

private:
    std::string name_;
    MemModel model_;
    ListSymtab members_;
    std::vector<int> memOff_;
    SymbolTable* whereDefined_;
    size_t alignAt_;
};

class IncompleteType : public Type
{
    // for undefined struct and union
public:
    explicit IncompleteType(std::string name, CompoundType::MemModel m,
                            SymbolTable* where)
            : Type(Incomplete, 0), name_(std::move(name)), model_(m), whereDefined_(where) {}
    CompoundType::MemModel model() const { return model_; }

    std::string toString() const override
    {
        if (model_ == CompoundType::Compound_Struct)
            return std::string("(struct ") + name_ + ")";
        else
            return std::string("(union ") + name_ + ")";
    }

    bool equalUnqual(const std::shared_ptr<Type>& t) override;
//    int typeCode() const override { return (int)Category::User; }

    SymbolTable* whereDefined() { return whereDefined_; }
private:
    std::string name_;
    CompoundType::MemModel model_;
    SymbolTable* whereDefined_;
};



class UserDefinedTypeRef : public Type
{
public:
    UserDefinedTypeRef(std::string name, SymbolTable* t,
                       QualifierHolder qh, size_t w)
            : Type(UserDefined, qh), typeName_(std::move(name)), scope_(t)
    { width_ = w; }

    std::string toString() const override;

    bool equalUnqual(const std::shared_ptr<Type>& t) override;
	const std::string& typeName() { return typeName_; }
	SymbolTable& scope() { return *scope_; }
	std::shared_ptr<Type> shallowCopy() const override {
	    return std::make_shared<UserDefinedTypeRef>(typeName_, scope_, qualifiers_, width_);
	}
private:
    std::string typeName_;
    SymbolTable* scope_;
};

class FuncType : public Type
{
public:
    using container = std::vector<std::shared_ptr<Type>>;
	using iterator = container::iterator;

    FuncType(std::string, const std::shared_ptr<Type>& ret);

    std::shared_ptr<Type> retType() const { return retType_; };
    const std::string& name() const { return name_; }
    void addParam(const std::shared_ptr<Type>& p) { params_.push_back(p); }
    container::iterator begin() { return params_.begin(); }
    container::iterator end() { return params_.end(); }
    size_t paramCount() { return params_.size(); }

    void defined(bool d) { defined_ = d; }
    bool defined() const { return defined_; }

	void hasVarArgs(bool h) { hasVarArgs_ = h; }
	bool hasVarArgs() const { return hasVarArgs_; }

    std::string toString() const override;


    bool equalUnqual(const std::shared_ptr<Type>& t) override;
//    int typeCode() const override { return (int)Category::Fn; }
private:
    std::string name_;
    std::shared_ptr<Type> retType_;
    std::vector<std::shared_ptr<Type>> params_;
    bool defined_ = false;
	bool hasVarArgs_ = false;
};

// TODO : add support to enum

class AliasType : public Type
{
public:
    explicit AliasType(std::string, const std::shared_ptr<Type>&);
    std::shared_ptr<Type> base() const;

    std::string toString() const override { return "" ;};

    bool equalUnqual(const std::shared_ptr<Type>& t) override { /*TODO : implement*/ return false;};
private:
    std::string name_;
    std::shared_ptr<Type> base_;
};

class EnumType : public Type
{
public:
    using value_type = std::pair<std::string, int>;
    EnumType(std::string, std::initializer_list<value_type>);
    int value(const std::string&) const;

    std::string toString() const override {return "";};

    bool equalUnqual(const std::shared_ptr<Type>& t) override {/*TODO : implement*/return false;};

private:
    std::string name_;
    std::vector<value_type> members_;
};
#endif