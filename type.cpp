#include "type.h"
#include <cassert>
#include <algorithm>
#include <numeric>
#include "platform.h"

namespace {

size_t alignUp(size_t v, size_t a) {
    // TODO to be improved
    while (v % a != 0)
        ++v;
    return v;
}

}

Type::Type(Tag t, QualifierHolder qh)
    : tag_(t), qualifiers_(qh)
{
    assert(qh <= 7);
}

Type::Tag Type::tag() const
{
    return tag_;
}

bool Type::isConst() const
{
    if (tag() != Array)
        return qualifiers_ & Const;
    else {
        Type const* tp = this;
        while (tp->tag() == Array) {
            tp = static_cast<ArrayType const*>(tp)->base().get();
        }
        return tp->isConst();
    }
}

void Type::isConst(bool b)
{
    if (b) {
        qualifiers_ |= Const;
    } else {
        qualifiers_ &= ~(uint8_t(Const));
    }
}

bool Type::isInteger(const std::shared_ptr<Type>& ty)
{
    return ty->tag() == BuiltIn
           && static_cast<BuiltInType*>(ty.get())->isInteger();
}

bool Type::isVoid(const std::shared_ptr<Type>& ty)
{
    return ty->tag() == BuiltIn
        && static_cast<BuiltInType*>(ty.get())->cat() == BuiltInType::Void;
}

bool Type::isArithmetic(const std::shared_ptr<Type>& ty)
{
    return ty->tag() == BuiltIn
        && static_cast<BuiltInType*>(ty.get())->cat() != BuiltInType::Void;
}

std::shared_ptr<PointerType> Type::arrayDecay(const std::shared_ptr<Type>& ty)
{
    assert(ty->tag() == Type::Pointer);
    PointerType* ptrTy = static_cast<PointerType*>(ty.get());
    return std::make_shared<PointerType>(ptrTy->base(), Const);
}

bool Type::isPointer(const std::shared_ptr<Type>& ty)
{
    return ty->tag() == Pointer;
}

size_t Type::alignAt(const std::shared_ptr<Type>& ty)
{
    if (ty->tag() == UserDefined) {
        auto t = static_cast<UserDefinedTypeRef*>(ty.get());
        auto ent = t->scope().find(t->typeName());
        if (ent->type->tag() == Type::Compound) {
            return static_cast<CompoundType*>(ent->type.get())->alignAt();
        } else {
            assert("other user defined types are not implemented" && false);
        }
    } else if (ty->tag() == Array) {
        return alignAt(static_cast<ArrayType*>(ty.get())->base());
    } else {
        return ty->width();
    }
}

bool Type::equal(const std::shared_ptr<Type>& t)
{
    if (tag() != Array)
        return equalUnqual(t) && qualifiers_ == t->qualifiers_;
    else {
        if (t->tag() != Array) {
            return false;
        }

        auto rty = static_cast<ArrayType*>(t.get());
        auto lty = static_cast<ArrayType*>(this);
        while (true) {
            if (lty->len() != rty->len()) {
                return false;
            }
            if (lty->base()->tag() == rty->base()->tag()) {
                if (lty->base()->tag() == Array) {
                    lty = static_cast<ArrayType*>(lty->base().get());
                    rty = static_cast<ArrayType*>(rty->base().get());
                } else {
                    return lty->base()->equal(rty->base());
                }
            } else {
                return false;
            }
        }
        assert(false);
    }
}

bool Type::isFloating(const std::shared_ptr<Type>& ty)
{
    return ty->tag() == BuiltIn
           && static_cast<BuiltInType*>(ty.get())->isFloating();
}

std::shared_ptr<Type> Type::derefIfIsUserDefinedType(const std::shared_ptr<Type>& ty)
{
    if (ty->tag() != Type::UserDefined)
        return ty;

    auto p = static_cast<UserDefinedTypeRef*>(ty.get());
    auto ent = p->scope().find(p->typeName());
    return ent->type;
}

bool Type::isScalar(const std::shared_ptr<Type>& ty)
{
    auto toTest = derefIfIsUserDefinedType(ty);
    return Type::isArithmetic(toTest) || Type::isPointer(toTest)
            || toTest->tag() == Type::Enum;
}

bool Type::isArray(const std::shared_ptr<Type>& ty)
{
    return ty->tag() == Type::Array;
}

// null means error: incompatible type specifiers
std::shared_ptr<BuiltInType> BuiltInType::make(SpecifierHolder spec,
                                               QualifierHolder qh)
{
    bool isint = !!(spec & BI_Int);
    bool ischar = !!(spec & BI_Char);
    bool isdouble = !!(spec & BI_Double);
    bool isfloat = !!(spec & BI_Float);
    bool isvoid = !!(spec & BI_VOID);
    bool isunsigned = !!(spec & BI_Unsigned);
    bool isshort = !!(spec & BI_Short);
    bool islong = !!(spec & BI_Long);

    if (isfloat) {
        if (isdouble || isint || ischar || isvoid
                || isunsigned || isshort || islong) {
            return nullptr;
        }
        return std::make_shared<BuiltInType>(Floating, false, SINGLESIZE, qh);
    }
    if (isdouble) {
        if (isint || ischar || isvoid
            || isunsigned || isshort || islong) {
            return nullptr;
        }
        return std::make_shared<BuiltInType>(Floating, false, DOUBLESIZE, qh);
    }
    if (isvoid) {
        if (isint || ischar
            || isunsigned || isshort || islong) {
            return nullptr;
        }
        return std::make_shared<BuiltInType>(Void, false, 0, qh);
    }

    // then integer only
    if (ischar) {
        if (isint || islong || isshort) {
            return nullptr;
        }

        return std::make_shared<BuiltInType>(Integer, isunsigned, CHARSIZE, qh);
    } else {
        // default : int
        if (isint) {/*ignored*/}
        if (islong) {
            if (isshort) {
                return nullptr;
            }
            return std::make_shared<BuiltInType>(Integer, isunsigned, PTRSIZE, qh);
        } else if (isshort) {
            return std::make_shared<BuiltInType>(Integer, isunsigned, SHORTSIZE, qh);
        } else {
            return std::make_shared<BuiltInType>(Integer, isunsigned, INTSIZE, qh);
        }
    }
}

std::string BuiltInType::toString() const
{
    std::string ret{"("};
    switch (cat_) {
    case Void: ret += "void"; break;
    case Integer:
        if (isUnsigned())
            ret += "u";
        ret += "i";
        ret += std::to_string(width());
        break;
    case Floating:
        ret += "floating";
        ret += std::to_string(width());
        break;
    default: return "errorBuiltInType";
    }
    if (isConst()) {
        ret.append(" const");
    }
    ret.append(")");
    return ret;
}

bool BuiltInType::equalUnqual(const std::shared_ptr<Type>& t)
{
    if (t->tag() != BuiltIn) {
        return false;
    }

    auto ty = static_cast<BuiltInType*>(t.get());
    return this->cat_ == ty->cat_ && isUnsigned_ == ty->isUnsigned_;
}

BuiltInType::BuiltInType(BuiltInType::Category cat, bool isUnsigned, size_t width, Type::QualifierHolder qh)
    : Type(BuiltIn, qh), cat_(cat), isUnsigned_(isUnsigned)
{
    assert(cat_ == Void || cat_ == Integer || cat_ == Floating);
    width_ = width;
    if (cat_ != Integer) {
        isUnsigned_ = false;
    }
}

std::shared_ptr<BuiltInType> BuiltInType::builtinClone() const
{
    return std::make_shared<BuiltInType>(cat_, isUnsigned_, width_, qualifiers_);
}

std::shared_ptr<BuiltInType> BuiltInType::maxUIntType()
{
    static auto v = std::make_shared<BuiltInType>
            (BuiltInType::Integer, true, PTRSIZE, 0);
    return v;
}

std::shared_ptr<BuiltInType> BuiltInType::voidType()
{
    static auto v = std::make_shared<BuiltInType>
            (BuiltInType::Void, false, 0, 0);
    return v;
}

std::shared_ptr<BuiltInType> BuiltInType::intType()
{
    static auto v = std::make_shared<BuiltInType>(BuiltInType::Integer, false, INTSIZE, 0);
    return v;
}

std::shared_ptr<BuiltInType> BuiltInType::doubleType()
{
    static auto v = std::make_shared<BuiltInType>(BuiltInType::Floating, false, DOUBLESIZE, 0);
    return v;
}

std::shared_ptr<BuiltInType> BuiltInType::charType()
{
    static auto v = std::make_shared<BuiltInType>(BuiltInType::Integer, false, CHARSIZE, 0);
    return v;
}

std::shared_ptr<BuiltInType> BuiltInType::uintType()
{
    static auto v = std::make_shared<BuiltInType>(BuiltInType::Integer, true, INTSIZE, 0);
    return v;
}

PointerType::PointerType(const std::shared_ptr<Type>& ty, QualifierHolder qh)
    : Type(Pointer, qh), base_(ty)
{
    width_ = PTRSIZE;
}

std::shared_ptr<Type> PointerType::base() const
{
    return base_; // xiajiba error
}

std::string PointerType::toString() const
{
    auto ret = std::string("(ptr2") + base_->toString();
    if (isConst()) {
        ret.append(" const");
    }
    ret.append(")");
    return ret;
}

bool PointerType::equalUnqual(const std::shared_ptr<Type>& t)
{
    if (t->tag() != Pointer) {
        return false;
    }

    auto ty = static_cast<PointerType*>(t.get());
    return base_->equal(ty->base_);
}

std::shared_ptr<PointerType> PointerType::strLiteralType()
{
    static auto v = std::make_shared<PointerType>(
            std::make_shared<BuiltInType>(BuiltInType::Integer, false, CHARSIZE, Const),
            0
    );
    return v;
}

ArrayType::ArrayType(const std::shared_ptr<Type>& ty, size_t len,
                     QualifierHolder qh)
        : Type(Array, qh), base_(ty), len_(len)
{
    width_ = ty->width() * len;
}

ArrayType::ArrayType(size_t len, Type::QualifierHolder qh)
        : Type(Array, qh), len_(len)
{

}

std::shared_ptr<Type> ArrayType::base() const
{
    return base_;
}

size_t ArrayType::len() const
{
    return len_;
}

std::string ArrayType::toString() const
{
    std::string ret = std::string("([") + std::to_string(len_) + "](";
    ret.append(base_->toString() + "))");
    return ret;
}

void ArrayType::base(std::shared_ptr<Type> ty)
{
    base_ = ty;
}



bool ArrayType::equalUnqual(const std::shared_ptr<Type>& rhs)
{
    if (rhs->tag() != Array) {
        return false;
    }

    auto rty = static_cast<ArrayType*>(rhs.get());
    auto lty = this;
    while (true) {
        if (lty->len() != rty->len()) {
            return false;
        }
        if (lty->base()->tag() == rty->base()->tag()) {
            if (lty->base()->tag() == Array) {
                lty = static_cast<ArrayType*>(lty->base().get());
                rty = static_cast<ArrayType*>(rty->base().get());
            } else {
                return lty->base()->equalUnqual(rty->base());
            }
        } else {
            return false;
        }
    }
    assert(false);
}


CompoundType::CompoundType(std::string n, MemModel m, SymbolTable* scope, ListSymtab&& members)
    : Type(Compound, 0), name_(std::move(n)), model_(m), whereDefined_(scope),
      members_(std::move(members))
{
    size_t maxAlign = 0;
    if (members_.begin() == members_.end()) {
        width_= 1;
        alignAt_ = 1;
        return;
    }

    if (model() == Compound_Union) {
        width_ = 0;
        for (auto& m : members_) {
            auto& ent = m.second;
            size_t a = Type::alignAt(ent.type);
            maxAlign = std::max(maxAlign, a);
            width_ = std::max(width_, ent.type->width());
        }
    } else {
        assert(model() == Compound_Struct);
        size_t offset = 0;
        for (auto& m : members_) {
            auto& ent = m.second;
            size_t a = Type::alignAt(ent.type);
            maxAlign = std::max(maxAlign, a);
            offset = alignUp(offset, a);
            ent.offset = offset;
            offset += ent.type->width();
        }
        // at last
        offset = alignUp(offset, maxAlign);
        width_ = offset;
    }
    alignAt_ = maxAlign;
}


auto CompoundType::begin() -> ListSymtab::iterator
{
    return members_.begin();
}

auto CompoundType::end() -> ListSymtab::iterator
{
    return members_.end();
}

std::string CompoundType::toString() const
{
    std::string ret = "(w";
    ret += std::to_string(width());
    if (model_ == Compound_Struct) {
        ret += std::string(" struct ") + name_ + " { ";
    } else {
        ret += std::string(" union ") + name_ + " { ";
    }

    for (auto it = members_.cbegin(); it != members_.cend(); ++it) {
        ret.append(it->second.type->toString() + " " + it->first + "; ");
    }
    ret.append("};)");
    return ret;
}

bool CompoundType::equalUnqual(const std::shared_ptr<Type>& t)
{
    if (t->tag() != Compound) {
        return false;
    }

    auto ty = static_cast<CompoundType*>(t.get());
    return name_ == ty->name_ && whereDefined_ == ty->whereDefined_;
}

/*std::vector<int>& CompoundType::memOff()
{
    if (width_ == -1)
        width();
    return memOff_;
}*/

AliasType::AliasType(std::string n, const std::shared_ptr<Type>& ty)
    : Type(Alias, 0), name_(std::move(n)), base_(ty)
{}

std::shared_ptr<Type> AliasType::base() const
{
    return base_;
}

EnumType::EnumType(std::string n, std::initializer_list<value_type> li)
    : Type(Enum, 0), name_(std::move(n)), members_(li.begin(), li.end())
{}

int EnumType::value(const std::string& name) const
{
    for (const auto& item : members_) {
        if (item.first == name) {
            return item.second;
        }
    }
    assert(false);
}


/*FuncType::FuncType(std::string n, const std::shared_ptr<Type>& ret,
                             std::initializer_list<std::shared_ptr<Type>> li)
    : Type(Func, 0), name_(std::move(n)), retType_(ret), params_(li.begin(), li.end())
{
}*/

FuncType::FuncType(std::string n, const std::shared_ptr<Type>& ret)
        : Type(Func, 0), name_(std::move(n)), retType_(ret)
{
}

std::string FuncType::toString() const
{
    std::string ret = std::string("(func ") + name_ + " -> " + retType_->toString() + " (";
    for (const auto& param : params_) {
        ret += param->toString();
        ret += ", ";
    }
    if (hasVarArgs_) {
        ret += "...";
    }
    ret += "))";
    return ret;
}

bool FuncType::equalUnqual(const std::shared_ptr<Type>& t)
{
    if (t->tag() != Func) {
        return false;
    }

    auto ty = static_cast<FuncType*>(t.get());
    bool eq = name_ == ty->name_;
    eq = eq && std::equal(params_.begin(), params_.end(),
                          ty->params_.begin(), ty->params_.end(),
                          [](const std::shared_ptr<Type>& lhs, const std::shared_ptr<Type>& rhs) {
                              return lhs->equal(rhs);
                          });

    return eq;
}

std::string UserDefinedTypeRef::toString() const
{
    std::string ret;
    auto p = scope_->find(typeName_);
    if (!p)
        ret = "(userdefnotfound";
    else {
        std::string defed;
        if (p->type->tag() == Incomplete) {
            defed = "incomplete: ";
            defed += p->type->toString();
        } else if (p->type->tag() == Compound) {
            auto cp = static_cast<CompoundType*>(p->type.get());
            if (cp->model() == CompoundType::Compound_Struct) {
                defed = "struct ";
            } else {
                defed = "union ";
            }
            defed += cp->name();
        } else {
            defed = "fuckingdontknow";
        }
        ret = std::string("(userdef:") + defed;
    }
    ret.append(")");
    return ret;
}

bool UserDefinedTypeRef::equalUnqual(const std::shared_ptr<Type>& t)
{
    if (t->tag() != UserDefined) {
        return false;
    }

    auto ty = static_cast<UserDefinedTypeRef*>(t.get());
    return typeName_ == ty->typeName_ && scope_ == ty->scope_;
}

bool IncompleteType::equalUnqual(const std::shared_ptr<Type>& t)
{
    if (t->tag() != Incomplete) {
        return false;
    }

    auto ty = static_cast<IncompleteType*>(t.get());
    return name_ == ty->name_ && whereDefined_ == ty->whereDefined_;
}

/*std::shared_ptr<BuiltInType> Type::FU_SInt()
{
    static auto ret = std::make_shared<BuiltInType>(SInt, 0);
    return ret;
}

std::shared_ptr<BuiltInType> Type::FU_UInt()
{
    static auto ret = std::make_shared<BuiltInType>(UInt, 0);
    return ret;
}

std::shared_ptr<BuiltInType> Type::FU_SChar()
{
    static auto ret = std::make_shared<BuiltInType>(SChar, 0);
    return ret;
}

std::shared_ptr<PointerType> Type::FU_SCharCP()
{
    static auto ret = std::make_shared<PointerType>(
            std::make_shared<BuiltInType>(Type::SChar, BuiltInType::Qualifier::Const)
            , 0);
    return ret;
}

const int Type::TagToCat[9] =
        { User, *//*won't be used*//*-1, Arr, Ptr, User, User, User, User, Fn };

const int Type::PosNeg_ResultType[15] = {
        Void, SChar, UChar, SShort, UShort,
        SInt, UInt, SLong, ULong, Float, Double,
        Void, Void, Void, Void
};

const int Type::Not_ResultType[15] = {
        Void, ULong, ULong, ULong, ULong,
        ULong, ULong, ULong, ULong, ULong, ULong,
        ULong, Void, Void, Void
};

const int Type::BitInv_ResultType[15] = {
        Void, SChar, UChar, SShort, UShort,
        SInt, UInt, SLong, ULong, Void, Void,
        Void, Void, Void, Void
};*/


