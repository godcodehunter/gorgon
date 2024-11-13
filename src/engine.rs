use winnow::{
    combinator::{alt, delimited, dispatch, empty, fail, opt, repeat},
    error::{ErrMode, ErrorKind, ParseError, ParserError},
    stream::{AsChar, Stream},
    token::*,
    PResult, Parser,
};

#[derive(Debug)]
pub enum Statement {
    RegularQuery(RegularQuery),
    StandaloneCall(StandaloneCall),
}

#[derive(Debug)]
pub struct RegularQuery {
    pub single_query: SingleQuery,
    pub r#union: Option<Union>,
}

#[derive(Debug)]
pub enum SingleQuery {
    SinglePartQuery(SinglePartQuery),
    MultiPartQuery(MultiPartQuery),
}

#[derive(Debug)]
pub enum SinglePartQuery {
    SinglePartShortQuery(SinglePartShortQuery),
    SinglePartLongQuery(SinglePartLongQuery),
}

#[derive(Debug)]
pub struct SinglePartShortQuery {
    pub reading_clause: Vec<ReadingClause>,
    pub r#return: Return,
}

#[derive(Debug)]
pub enum ReadingClause {
    Match(Match),
    Unwind(Unwind),
    InQueryCall(InQueryCall),
}

#[derive(Debug)]
pub struct Match {
    pub optional: bool,
    pub pattern: Vec<PatternPart>,
    pub r#where: Option<Where>,
}

#[derive(Debug)]
pub struct Where(Expression);

#[derive(Debug)]
pub struct Unwind {
    pub expression: Expression,
    pub variable: SymbolicName,
}

#[derive(Debug)]
pub struct InQueryCall;

#[derive(Debug)]
pub struct Return {
    pub data: ProjectionBody,
}

#[derive(Debug)]
struct ProjectionBody {
    pub projection_items: ProjectionItems,
    pub order: Order,
    // skip: ,
    pub limit: Limit,
}

#[derive(Debug)]
pub struct ProjectionItems;

#[derive(Debug)]
pub struct Limit(Expression);

#[derive(Debug)]
pub struct Order(Vec<SortItem>);

#[derive(Debug)]
pub struct SortItem;

#[derive(Debug)]
pub struct SinglePartLongQuery {
    pub reading_clause: Vec<ReadingClause>,
    pub updating_clause: Vec<UpdatingClause>,
    pub r#return: Option<Return>,
}

#[derive(Debug)]
pub struct MultiPartQuery {
    pub reading_clause: Option<ReadingClause>,
    pub updating_clause: UpdatingClause,
    pub single_part_query: SinglePartQuery,
    pub with: Option<With>,
}

#[derive(Debug)]
pub struct With {
    pub projection_body: ProjectionBody,
    pub r#where: Option<Where>, 
}

#[derive(Debug)]
pub enum UpdatingClause {
    Create(Vec<PatternPart>),
    Merge(Merge),
    Delete(Delete),
    Set(Set),
    Remove(Remove),
}

#[derive(Debug)]
pub struct Merge {
    pub pattern_part: PatternPart,
    pub merge_action: Option<MergeAction>,
}

#[derive(Debug)]
pub struct PatternPart;

#[derive(Debug)]
pub enum MergeAction {
    Match(Set),
    Create(Set),
}

#[derive(Debug)]
pub struct Delete;

#[derive(Debug)]
pub struct Set(Vec<SetItem>);

#[derive(Debug)]
pub struct SetItem;

#[derive(Debug)]
pub struct Remove;

#[derive(Debug)]
pub enum Union {
    Trivial(SingleQuery),
    All(SingleQuery),
}

#[derive(Debug)]
pub struct StandaloneCall;

#[derive(Debug)]
pub enum SymbolicName {
    UnescapedSymbolicName(String),
    EscapedSymbolicName(String),
    HexLetter(char),
    Count,
    Filter,
    Extract,
    Any,
    None,
    Single,
}

#[derive(Debug)]
pub enum Parameter {
    SymbolicName(SymbolicName),
    DecimalInteger(isize),
}

#[derive(Debug)]
enum Atom {
    Literal(Literal),
    Parameter(Parameter),
    CaseExpression(CaseExpression),
    Count,
    ListComprehension(ListComprehension),
    Quantifier(Quantifier),
    PatternPredicate(PatternPredicate),
    ParenthesizedExpression(ParenthesizedExpression),
    FunctionInvocation(FunctionInvocation),
    ExistentialSubquery(ExistentialSubquery),
    Variable(SymbolicName),
}

#[derive(Debug)]
pub struct ExistentialSubquery;

#[derive(Debug)]
pub struct FunctionInvocation;

#[derive(Debug)]
pub struct ParenthesizedExpression;

#[derive(Debug)]
pub struct PatternPredicate;

#[derive(Debug)]
pub struct Quantifier;

#[derive(Debug)]
pub struct ListComprehension;

#[derive(Debug)]
pub struct CaseExpression;

#[derive(Debug)]
pub struct MapLiteral {
    data: Vec<(SymbolicName, Expression)>,
}

#[derive(Debug)]
pub struct Expression(OrExpression);

#[derive(Debug)]
pub struct OrExpression;

#[derive(Debug)]
enum Literal {
    BooleanLiteral(bool),
    Null,
    NumberLiteral(NumberLiteral),
    StringLiteral(String),
    ListLiteral(Vec<Expression>),
    MapLiteral(MapLiteral),
}

#[derive(Debug)]
enum NumberLiteral {
    Integer(isize),
    Double(f64),
}

#[derive(Debug)]
struct YieldItem {
    pub procedure_field: Option<SymbolicName>,
    pub variable: SymbolicName,
}

#[derive(Debug)]
struct ProcedureResultField;

#[derive(Debug)]
struct ProcedureName {
    pub namespace: Option<SymbolicName>,
    pub name: SymbolicName,
}

#[derive(Debug)]
struct YieldItems {
    items: Vec<YieldItem>,
    r#where: Option<Where>,
}


#[allow(dead_code)]
pub fn parse_chypher<'s>(s: &mut &'s str) -> PResult<Statement> {
    let result = (
        opt(parse_whitespace),
        parse_statement,
        opt((parse_whitespace, literal(';'))),
        opt(parse_whitespace),
    )
        .parse_next(s);

    let (_, stmt, _, _) = result?;
    Ok(stmt)
}

fn parse_statement<'s>(s: &mut &'s str) -> PResult<Statement> {
    parse_query(s)
}

fn parse_query<'s>(s: &mut &'s str) -> PResult<Statement> {
    alt((
        parse_regular_query.map(|rq| Statement::RegularQuery(rq)),
        parse_standalone_call.map(|sc| Statement::StandaloneCall(sc)),
    ))
    .parse_next(s)
}

fn parse_regular_query<'s>(s: &mut &'s str) -> PResult<RegularQuery> {
    let result = (parse_single_query, opt((parse_whitespace, parse_union))).parse_next(s);

    let (sq, opt) = result?;

    Ok(RegularQuery {
        single_query: sq,
        union: opt.map(|v| v.1),
    })
}

fn parse_union<'s>(s: &mut &'s str) -> PResult<Union> {
    alt((
        parse_trivial_union.map(|v| Union::Trivial(v)),
        parse_all_union.map(|v| Union::All(v)),
    ))
    .parse_next(s)
}

// synthetic
fn parse_trivial_union<'s>(s: &mut &'s str) -> PResult<SingleQuery> {
    let result = (literal("UNION"), opt(parse_whitespace), parse_single_query).parse_next(s);

    let (_, _, sq) = result?;
    Ok(sq)
}

// synthetic
fn parse_all_union<'s>(s: &mut &'s str) -> PResult<SingleQuery> {
    let result = (
        literal("UNION"),
        parse_whitespace,
        literal("ALL"),
        opt(parse_whitespace),
        parse_single_query,
    )
        .parse_next(s);

    let (_, _, _, _, sq) = result?;
    Ok(sq)
}

fn parse_single_query<'s>(s: &mut &'s str) -> PResult<SingleQuery> {
    alt((
        parse_single_part_query.map(|v| SingleQuery::SinglePartQuery(v)),
        parse_multi_part_query.map(|v| SingleQuery::MultiPartQuery(v)),
    ))
    .parse_next(s)
}

fn parse_single_part_query<'s>(s: &mut &'s str) -> PResult<SinglePartQuery> {
    alt((
        parse_single_part_query_short.map(|v| SinglePartQuery::SinglePartShortQuery(v)),
        parse_single_part_query_long.map(|v| SinglePartQuery::SinglePartLongQuery(v)),
    ))
    .parse_next(s)
}

// synthetic
fn parse_single_part_query_short<'s>(s: &mut &'s str) -> PResult<SinglePartShortQuery> {
    let result = (
        repeat(0.., (parse_reading_clause, opt(parse_whitespace))),
        parse_return,
    )
        .parse_next(s);

    let (pc, ret): (Vec<_>, Return) = result?;

    let collected = pc.into_iter().map(|v| v.0).collect();

    Ok(SinglePartShortQuery {
        reading_clause: collected,
        r#return: ret,
    })
}

// synthetic
fn parse_single_part_query_long<'s>(s: &mut &'s str) -> PResult<SinglePartLongQuery> {
    let result = (
        repeat(0.., (parse_reading_clause, opt(parse_whitespace))),
        parse_updating_clause,
        repeat(0.., (opt(parse_whitespace), parse_updating_clause)),
        opt((opt(parse_whitespace), parse_return)),
    )
        .parse_next(s);

    let (rc, uc, uc_vec, ret): (Vec<_>, UpdatingClause, Vec<_>, Option<_>) = result?;

    let rc = rc.into_iter().map(|v| v.0).collect();

    let mut uc_acc = Vec::new();
    uc_acc.push(uc);

    let mut tmp_uc: Vec<_> = uc_vec.into_iter().map(|v| v.1).collect();
    uc_acc.append(&mut tmp_uc);

    Ok(SinglePartLongQuery {
        reading_clause: rc,
        updating_clause: uc_acc,
        r#return: ret.map(|v| v.1),
    })
}

fn parse_multi_part_query<'s>(s: &mut &'s str) -> PResult<MultiPartQuery> {
    let result = (
        repeat(
            0..,
            (
            repeat(0.., (parse_reading_clause, opt(parse_whitespace))),
            repeat(0.., (parse_updating_clause, opt(parse_whitespace))),
            parse_with,
            opt(parse_whitespace),
        )),
        parse_single_part_query,
    )
        .parse_next(s);

    let (opt, sq): (Vec<(Vec<_>, Vec<_>, _, _)>, SinglePartQuery) = result?;


    for item in opt {
        // TODO
    }

   todo!()
}

fn parse_updating_clause<'s>(s: &mut &'s str) -> PResult<UpdatingClause> {
    alt((
        parse_create.map(|v| UpdatingClause::Create(v)),
        parse_merge.map(|v| UpdatingClause::Merge(v)),
        parse_delete.map(|v| UpdatingClause::Delete(v)),
        parse_set.map(|v| UpdatingClause::Set(v)),
        parse_remove.map(|v| UpdatingClause::Remove(v)),
    ))
    .parse_next(s)
}

fn parse_reading_clause<'s>(s: &mut &'s str) -> PResult<ReadingClause> {
    alt((
        parse_match.map(|v| ReadingClause::Match(v)),
        parse_unwind.map(|v| ReadingClause::Unwind(v)),
        parse_in_query_call.map(|v| ReadingClause::InQueryCall(v)),
    ))
    .parse_next(s)
}

fn parse_match<'s>(s: &mut &'s str) -> PResult<Match> {
    let result = (
        opt((literal("OPTIONAL"), parse_whitespace)),
        literal("MATCH"),
        opt(parse_whitespace),
        parse_pattern,
        opt((opt(parse_whitespace), parse_where)),
    )
        .parse_next(s);
    let (opt_kw, _, _, pat, opt_where) = result?;

    let wh = opt_where.map(|v| v.1);

    Ok(Match {
        optional: opt_kw.is_some(),
        pattern: pat,
        r#where: wh,
    })
}

fn parse_unwind<'s>(s: &mut &'s str) -> PResult<Unwind> {
    let result = (
        literal("UNWIND"),
        opt(parse_whitespace),
        parse_expression,
        parse_whitespace,
        literal("AS"),
        parse_whitespace,
        parse_variable,
    ).parse_next(s)?;

    Ok(Unwind {
        expression: result.2,
        variable: result.6,
    })
}

fn parse_merge<'s>(s: &mut &'s str) -> PResult<Merge> {
    let result = (
        literal("MERGER"),
        opt(parse_whitespace),
        parse_pattern_part,
        opt((parse_whitespace, parse_merge_action)),
    )
        .parse_next(s);

    let (_, _, pt, ma) = result?;

    Ok(Merge {
        pattern_part: pt,
        merge_action: ma.map(|v| v.1),
    })
}

fn parse_merge_action<'s>(s: &mut &'s str) -> PResult<MergeAction> {
    let convert1 = |(_, _, _, _, set)| {
        MergeAction::Match(set)
    };
    
    let convert2 = |(_, _, _, _, set)| {
        MergeAction::Create(set)
    };
    
    alt((
        (literal("ON"), parse_whitespace, literal("MATCH"), parse_whitespace, parse_set).map(convert1),
        (literal("ON"), parse_whitespace, literal("CREATE"), parse_whitespace, parse_set).map(convert2),
    )).parse_next(s)
}

fn parse_create<'s>(s: &mut &'s str) -> PResult<Vec<PatternPart>> {
    let result = (
        literal("CREATE"), 
        opt(parse_whitespace), 
        parse_pattern,
    ).parse_next(s)?;

    Ok(result.2)
}

fn parse_set<'s>(s: &mut &'s str) -> PResult<Set> {
    let result = (
        literal("SET"),
        opt(parse_whitespace),
        parse_set_item,
        repeat(0.., (
            opt(parse_whitespace),
            literal(","),
            opt(parse_whitespace),
            parse_set_item
        ))
    ).parse_next(s);

    let (_, _, item, rep): (_, _, _, Vec<_>) = result?;
    let mut acc = Vec::new();
    acc.push(item);
    let mut tmp = rep.into_iter().map(|v| v.3).collect();
    acc.append(&mut tmp);

    Ok(Set(acc))
}

fn parse_set_item<'s>(s: &mut &'s str) -> PResult<SetItem> {
    todo!()
}

fn parse_delete<'s>(s: &mut &'s str) -> PResult<Delete> {
    todo!()
}

fn parse_remove<'s>(s: &mut &'s str) -> PResult<Remove> {
    todo!()
}

fn parse_remove_item<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_in_query_call<'s>(s: &mut &'s str) -> PResult<InQueryCall> {
    todo!()
}

fn parse_standalone_call<'s>(s: &mut &'s str) -> PResult<StandaloneCall> {
    todo!()
}

fn parse_yield_items<'s>(s: &mut &'s str) -> PResult<YieldItems> {
    let result = (
        parse_yield_item,
        repeat(0.., (
            opt(parse_whitespace),
            literal(","),
            opt(parse_whitespace),
            parse_yield_item
        )),
        opt((
            opt(parse_whitespace),
            parse_where,
        ))
    ).parse_next(s);

    let (item, rep, owh): (_, Vec<_>, _) = result?;
    let mut acc = Vec::new();
    acc.push(item);
    let mut tmp: Vec<_> = rep.into_iter().map(|v| v.3).collect();
    acc.append(&mut tmp);

    let mut r#where = None;
    if let Some((_, w)) = owh {
        r#where = Some(w);
    }

    Ok(YieldItems { items: acc, r#where })
}

fn parse_yield_item<'s>(s: &mut &'s str) -> PResult<YieldItem> {
    let result = (
        opt((
            parse_procedure_result_field,
            parse_whitespace,
            literal("AS"),
            parse_whitespace
        )),
        parse_variable
    ).parse_next(s);

    let (opt, variable) = result?;

    Ok(YieldItem {
        procedure_field: opt.map(|v| v.0),
        variable,
    })
}

fn parse_with<'s>(s: &mut &'s str) -> PResult<With> {
    let result = (
        literal("WITH"),
        parse_projection_dody,
        opt((
            opt(parse_whitespace),
            parse_where,
        ))
    ).parse_next(s);

    let (_, pb, opt) = result?;

    Ok(With {
        projection_body: pb,
        r#where: opt.map(|v| v.1),
    })
}

fn parse_return<'s>(s: &mut &'s str) -> PResult<Return> {
    let result = (
        literal("RETURN"),
        parse_projection_dody,
    ).parse_next(s);

    let (_, data) = result?;

    Ok(Return { data })
}

fn parse_projection_dody<'s>(s: &mut &'s str) -> PResult<ProjectionBody> {
    todo!()
}

fn parse_projection_items<'s>(s: &mut &'s str) -> PResult<ProjectionItems> {
    todo!()
}

fn parse_projection_item<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_order<'s>(s: &mut &'s str) -> PResult<Order> {
    let result = (
        literal("ORDER"), 
        parse_whitespace,
        literal("BY"),
        parse_whitespace,
        parse_sort_item,
        repeat(0.., (
            literal(","),
            opt(parse_whitespace),
            parse_sort_item,
        )) 
    ).parse_next(s);

    let (_, _, _, _, si, vec): (_, _, _, _, _, Vec<_>) = result?;
    let mut data = Vec::new();
    data.push(si);
    let mut c: Vec<_> = vec.into_iter().map(|v| v.2).collect();
    data.append(&mut c);

    Ok(Order(data))
}

fn parse_skip<'s>(s: &mut &'s str) -> PResult<Expression> {
    (literal("SKIP"), parse_whitespace, parse_expression)
    .parse_next(s)
    .map(|v|v.2)
}

fn parse_limit<'s>(s: &mut &'s str) -> PResult<Limit> {
    let result = (literal("LIMIT"), parse_whitespace, parse_expression).parse_next(s)?;
    Ok(Limit(result.2))
}

fn parse_sort_item<'s>(s: &mut &'s str) -> PResult<SortItem> {
    todo!()
}

fn parse_where<'s>(s: &mut &'s str) -> PResult<Where> {
    let result = (literal("WHERE"), parse_whitespace, parse_expression).parse_next(s)?;

    Ok(Where(result.2))
}

fn parse_pattern<'s>(s: &mut &'s str) -> PResult<Vec<PatternPart>> {
    let result = (
        parse_pattern_part,
        repeat(
            0..,
            (
                opt(parse_whitespace),
                literal(','),
                opt(parse_whitespace),
                parse_pattern_part,
            ),
        ),
    )
        .parse_next(s);

    let (fpp, rep): (PatternPart, Vec<_>) = result?;
    let mut v = Vec::new();
    v.push(fpp);
    for (_, _, _, pat) in rep {
        v.push(pat);
    }

    Ok(v)
}

fn parse_pattern_part<'s>(s: &mut &'s str) -> PResult<PatternPart> {
    alt((parse_long_pattern_pat, parse_anonymous_pattern_part)).parse_next(s);

    todo!()
}

// synthetic
fn parse_long_pattern_pat<'s>(s: &mut &'s str) -> PResult<&'s str> {
    (
        parse_variable,
        opt(parse_whitespace),
        literal('='),
        opt(parse_whitespace),
        parse_anonymous_pattern_part,
    )
        .parse_next(s);

    todo!()
}

fn parse_anonymous_pattern_part<'s>(s: &mut &'s str) -> PResult<&'s str> {
    parse_pattern_element(s)
}

fn parse_pattern_element<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_relationships_pattern<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_node_pattern<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_pattern_element_chain<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_relationship_pattern<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_relationship_detail<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_properties<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_relationship_types<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_node_labels<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_node_label<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_range_literal<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_label_name<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_rel_type_name<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_property_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_expression<'s>(s: &mut &'s str) -> PResult<Expression> {
    parse_or_expression(s).map(|or| Expression(or))
}

fn parse_or_expression<'s>(s: &mut &'s str) -> PResult<OrExpression> {
    todo!()
}

fn parse_xor_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_and_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_not_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_comparison_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_partial_comparison_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_string_list_null_predicate_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_string_predicate_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_list_predicate_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_null_predicate_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_add_or_subtract_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_multiply_divide_modulo_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_power_of_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_unary_add_or_subtract_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_non_arithmetic_operator_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_list_operator_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_property_lookup<'s>(s: &mut &'s str) -> PResult<SymbolicName> {
    let result = (
        literal("."), 
        opt(parse_whitespace), 
        parse_property_key_name,
    ).parse_next(s);

    let (_, _, kn) = result?;

    Ok(kn)
}

fn parse_atom<'s>(s: &mut &'s str) -> PResult<Atom> {
    alt((
        parse_literal.map(|v| Atom::Literal(v)),
        parse_parameter.map(|v| Atom::Parameter(v)),
        parse_case_expression.map(|v| Atom::CaseExpression(v)),
        parse_count.map(|v| Atom::Count),
        parse_list_comprehension.map(|v| Atom::ListComprehension(v)),
        parse_quantifier.map(|v| Atom::Quantifier(v)),
        parse_pattern_predicate.map(|v| Atom::PatternPredicate(v)),
        parse_parenthesized_expression.map(|v| Atom::ParenthesizedExpression(v)),
        parse_function_invocation.map(|v| Atom::FunctionInvocation(v)),
        parse_existential_subquery.map(|v| Atom::ExistentialSubquery(v)),
        parse_variable.map(|v| Atom::Variable(v)),
    )).parse_next(s)
}

// synthetic
fn parse_count<'s>(s: &mut &'s str) -> PResult<()> {
    (
        literal("COUNT"), 
        opt(parse_whitespace), 
        literal("("),
        opt(parse_whitespace), 
        literal("*"),
        opt(parse_whitespace),
        literal(")"),
    ).parse_next(s)
    .map(|v| ())
}

fn parse_case_expression<'s>(s: &mut &'s str) -> PResult<CaseExpression> {
    todo!()
}

fn parse_case_alternative<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_list_comprehension<'s>(s: &mut &'s str) -> PResult<ListComprehension> {
    todo!()
}

fn parse_pattern_comprehension<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_quantifier<'s>(s: &mut &'s str) -> PResult<Quantifier> {
    todo!()
}

fn parse_filter_expression<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_pattern_predicate<'s>(s: &mut &'s str) -> PResult<PatternPredicate> {
    todo!()
}

fn parse_parenthesized_expression<'s>(s: &mut &'s str) -> PResult<ParenthesizedExpression> {
    todo!()
}

fn parse_id_in_coll<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_function_invocation<'s>(s: &mut &'s str) -> PResult<FunctionInvocation> {
    todo!()
}

fn parse_function_name<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_existential_subquery<'s>(s: &mut &'s str) -> PResult<ExistentialSubquery> {
    todo!()
}

fn parse_explicit_procedure_invocation<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_implicitprocedure_invocation<'s>(s: &mut &'s str) -> PResult<ProcedureName> {
    parse_procedure_name(s)
}

fn parse_procedure_result_field<'s>(s: &mut &'s str) -> PResult<SymbolicName> {
    parse_symbolic_name(s)
}

fn parse_procedure_name<'s>(s: &mut &'s str) -> PResult<ProcedureName> {
    let result = (parse_namespace, parse_symbolic_name).parse_next(s)?;
    
    Ok(ProcedureName {
        namespace: result.0,
        name: result.1,
    })
}

fn parse_namespace<'s>(s: &mut &'s str) -> PResult<Option<SymbolicName>> {
    opt((parse_symbolic_name, literal(".")))
    .parse_next(s)
    .map(|v| v.map(|v| v.0))
}

fn parse_variable<'s>(s: &mut &'s str) -> PResult<SymbolicName> {
    parse_symbolic_name(s)
}

fn parse_literal<'s>(s: &mut &'s str) -> PResult<Literal> {
    alt((
        parse_boolean_literal.map(|v| Literal::BooleanLiteral(v)),
        literal("NULL").map(|v| Literal::Null),
        parse_number_literal.map(|v| Literal::NumberLiteral(v)),
        parse_string_literal.map(|v| Literal::StringLiteral(v)),
        parse_list_literal.map(|v| Literal::ListLiteral(v)),
        parse_map_literal.map(|v| Literal::MapLiteral(v))
    )).parse_next(s)
}

fn parse_boolean_literal<'s>(s: &mut &'s str) -> PResult<bool> {
    let result = alt((
        literal("TRUE"),
        literal("FALSE")
    )).parse_next(s);
    
    let v = result?;
    if v == "TRUE" {
        return Ok(true);
    }

    Ok(false)
}

fn parse_number_literal<'s>(s: &mut &'s str) -> PResult<NumberLiteral> {
    alt((
        parse_integer_literal.map(|v| NumberLiteral::Integer(v)),
        parse_double_literal.map(|v| NumberLiteral::Double(v)),
    )).parse_next(s)
}

fn parse_integer_literal<'s>(s: &mut &'s str) -> PResult<isize> {
    alt((
        parse_hex_integer,
        parse_octal_integer,
        parse_decimal_integer,
    )).parse_next(s)
}

fn parse_hex_integer<'s>(s: &mut &'s str) -> PResult<isize> {
    let result = (literal("0x"), repeat(1.., parse_hex_digit)).parse_next(s);
    let (_, dig): (_, Vec<_>) = result?;
    let str: String = dig.into_iter().collect();

    Ok(isize::from_str_radix(&str, 16).unwrap())
}

fn parse_decimal_integer<'s>(s: &mut &'s str) -> PResult<isize> {
    let to_digit = |(nz, vec) : (char, Vec<char>)| -> isize {
        let mut v = Vec::new();
        v.push(nz);
        v.append(&mut vec.clone());
        let str: String = v.into_iter().collect();
        isize::from_str_radix(&str, 10).unwrap()
    };

    alt((
        parse_zero_digit.map(|_| 0),
        (parse_non_zero_digit, repeat(0.., parse_digit)).map(to_digit)
    )).parse_next(s)
}

fn parse_octal_integer<'s>(s: &mut &'s str) -> PResult<isize> {
    let result = (literal("0o"), repeat(1.., parse_oct_digit)).parse_next(s);
    let (_, dig): (_, Vec<_>) = result?;
    let s: String = dig.into_iter().collect();

    Ok(isize::from_str_radix(&s, 8).unwrap())
}

fn parse_hex_letter<'s>(s: &mut &'s str) -> PResult<char> {
    dispatch! {any;
        'A' => empty.value('A'),
        'B' => empty.value('B'),
        'C' => empty.value('C'),
        'D' => empty.value('D'),
        'E' => empty.value('E'),
        'F' => empty.value('F'),
        _ => fail::<_, char, _>,
    }
    .parse_next(s)
}

fn parse_hex_digit<'s>(s: &mut &'s str) -> PResult<char> {
    alt((
        parse_digit,
        parse_hex_letter,
    )).parse_next(s)
}

fn parse_digit<'s>(s: &mut &'s str) -> PResult<char> {
    alt((
        parse_zero_digit,
        parse_non_zero_digit,
    )).parse_next(s)
}

fn parse_non_zero_digit<'s>(s: &mut &'s str) -> PResult<char> {
    dispatch! {any;
        '1' => empty.value('1'),
        '2' => empty.value('2'),
        '3' => empty.value('3'),
        '4' => empty.value('4'),
        '5' => empty.value('5'),
        '6' => empty.value('6'),
        '7' => empty.value('7'),
        '8' => empty.value('8'),
        '9' => empty.value('9'),
        _ => fail::<_, char, _>,
    }
    .parse_next(s)
}

fn parse_non_zero_oct_digit<'s>(s: &mut &'s str) -> PResult<char> {
    dispatch! {any;
        '1' => empty.value('1'),
        '2' => empty.value('2'),
        '3' => empty.value('3'),
        '4' => empty.value('4'),
        '5' => empty.value('5'),
        '6' => empty.value('6'),
        '7' => empty.value('7'),
        _ => fail::<_, char, _>,
    }
    .parse_next(s)
}

fn parse_oct_digit<'s>(s: &mut &'s str) -> PResult<char> {
    alt((
        parse_zero_digit,
        parse_non_zero_digit,
    )).parse_next(s)
}

fn parse_zero_digit<'s>(s: &mut &'s str) -> PResult<char> {
    let c = s.next_token().ok_or_else(|| {
        ErrMode::from_error_kind(s, ErrorKind::Token)
    })?;
    if c != '0' {
        return Err(ErrMode::from_error_kind(s, ErrorKind::Verify));
    }
    Ok(c)
}

fn parse_double_literal<'s>(s: &mut &'s str) -> PResult<f64> {
    alt((
        parse_exponent_decimal_real,
        parse_regular_decimal_real,
    )).parse_next(s)
}

fn parse_exponent_decimal_real<'s>(s: &mut &'s str) -> PResult<f64> {
    todo!()
}

fn parse_regular_decimal_real<'s>(s: &mut &'s str) -> PResult<f64> {
    let result = (
        repeat(0.., parse_digit), 
        literal("."),
        repeat(1.., parse_digit), 
    ).parse_next(s);

    let (lopt, _, ropt): (Vec<_>, _, Vec<_>) = result?;

    let number_str = format!(
        "{}.{}",
        lopt.iter().collect::<String>(),
        ropt.iter().collect::<String>()
    );

    Ok(number_str.parse::<f64>().unwrap())
}

// NOTE: partial impl
fn parse_string_literal<'s>(s: &mut &'s str) -> PResult<String> {
    todo!()
}

fn parse_escaped_char<'s>(s: &mut &'s str) -> PResult<&'s str> {
    todo!()
}

fn parse_list_literal<'s>(s: &mut &'s str) -> PResult<Vec<Expression>> {
    let result = (
        literal("["), 
        opt(parse_whitespace), 
        opt((
            parse_expression,
            opt(parse_whitespace),
            repeat(0.., (
                literal(","),
                opt(parse_whitespace),
                parse_expression,
                opt(parse_whitespace),
            ))
        )), 
        literal("]"),
    ).parse_next(s);

    let (_, _, opt, _): (_, _, Option<(_, _, Vec<_>)>, _) = result?;

    let mut v = Vec::new();
    if let Some(o) = opt {
        v.push(o.0);

        let mut v2 = o.2.into_iter().map(|v| v.2).collect();
        v.append(&mut v2);
    }

    Ok(v)
}

fn parse_map_literal<'s>(s: &mut &'s str) -> PResult<MapLiteral> {
    let result = (
        literal('{'),
        opt(parse_whitespace),
        opt((
            parse_property_key_name,
            opt(parse_whitespace),
            literal(":"),
            opt(parse_whitespace),
            parse_expression,
            opt(parse_whitespace),
            repeat(0.., (
                literal(','),
                opt(parse_whitespace),

                parse_property_key_name,
                opt(parse_whitespace),
                literal(":"),
                opt(parse_whitespace),
                parse_expression,
                opt(parse_whitespace)
            ))
        )),
        literal('}'),
    ).parse_next(s);

    let (
        _,
        _,
        opt,
        _
    ): (&str, Option<_>, Option<(SymbolicName, Option<_>, &str, Option<_>, Expression, Option<_>, Vec<_>)>, &str) = result?;

    let mut data = Vec::new();
    if let Some((s, _, _, _, expr, _, r)) = opt {
        data.push((s, expr));
        let mut v = r.into_iter().map(|v| (v.2, v.6)).collect::<Vec<_>>();
        data.append(&mut v);
    }

    Ok(MapLiteral{
        data,
    })
}

fn parse_property_key_name<'s>(s: &mut &'s str) -> PResult<SymbolicName> {
    parse_schema_name(s)
}

fn parse_parameter<'s>(s: &mut &'s str) -> PResult<Parameter> {
    let result = (
        literal("$"), 
        alt((
            parse_symbolic_name.map(|v| Parameter::SymbolicName(v)), 
            parse_decimal_integer.map(|v| Parameter::DecimalInteger(v)),
        ))
    ).parse_next(s);

    let (_, alt) = result?;

    Ok(alt)
}

// NOTE: is partial implemented, without keyword
fn parse_schema_name<'s>(s: &mut &'s str) -> PResult<SymbolicName> {
    parse_symbolic_name(s)
}

fn parse_symbolic_name<'s>(s: &mut &'s str) -> PResult<SymbolicName> {
    alt((
        parse_escaped_symbolic_name.map(|v| SymbolicName::EscapedSymbolicName(v.to_string())),
        parse_unescaped_symbolic_name.map(|v| SymbolicName::UnescapedSymbolicName(v.to_string())),
        parse_hex_letter.map(|v| SymbolicName::HexLetter(v)),
        literal("COUNT").map(|_| SymbolicName::Count),
        literal("FILTER").map(|_| SymbolicName::Filter),
        literal("EXTRACT").map(|_| SymbolicName::Extract),
        literal("ANY").map(|_| SymbolicName::Any),
        literal("NONE").map(|_| SymbolicName::None),
        literal("SINGLE").map(|_| SymbolicName::Single),
    )).parse_next(s)
}

fn parse_unescaped_symbolic_name<'s>(s: &mut &'s str) -> PResult<&'s str> {
    take_while(1.., AsChar::is_alphanum).parse_next(s)
}

// NOTE: is partial implemented, without backticks are escaped with double backticks.
fn parse_escaped_symbolic_name<'s>(s: &mut &'s str) -> PResult<&'s str> {
    let result = (
        literal("`"), 
        take_until(1.., "`")
    ).parse_next(s);

    let (_, tu) = result?;

    Ok(tu)
}

// NOTE: partial implemented set of symbol
fn parse_whitespace<'s>(s: &mut &'s str) -> PResult<()> {
    repeat(
        0..,
        alt((
            parse_comment,
            take_while(1.., AsChar::is_space).map(|_| ())
        ))        
    ).parse_next(s)
}

fn parse_comment<'s>(s: &mut &'s str) -> PResult<()> {
    alt((
        (
            literal("/*"), 
            take_until(0.., "*/")
        ),
        (
            literal("//"),
            take_till(0.., AsChar::is_newline)
        )
    )).parse_next(s)
    .map(|_| ())
}


fn parse_left_arrow_head<'s>(s: &mut &'s str) -> PResult<()> {
    dispatch! {any;
        '<' => (),
        '⟨' => (),
        '〈' => (),
        '﹤' => (),
        '＜' => (),
        _ => fail::<_, (), _>,
    }
    .parse_next(s)
}

fn parse_right_arrow_head<'s>(s: &mut &'s str) -> PResult<()> {
    dispatch! {any;
        '>' => (),
        '⟩' => (),
        '〉' => (),
        '﹥' => (),
        '＞' => (),
        _ => fail::<_, (), _>,
    }
    .parse_next(s)
}

fn parse_dash<'s>(s: &mut &'s str) -> PResult<()> {
    dispatch! {any;
        '-' => (),
         '­' => (),
        '‐' => (),
        '‑' => (),
        '‒' => (),
        '–' => (),
        '—' => (),
        '―' => (),
        '−' => (),
        '﹘' => (),
        '﹣' => (),
        '－' => (),
        _ => fail::<_, (), _>,
    }
    .parse_next(s)
}

#[test]
fn test_parse_match() {
    let mut input = r#"MATCH (node1:Label1) WHERE node1.propertyA = $value"#;

    let result = parse_match(&mut input);
    dbg!(result);
}

#[test]
fn test_parse_regular_decimal_real() {
    let mut input = ".34";

    let result =  parse_regular_decimal_real(&mut input);
    assert_eq!(result.unwrap(), 0.34);

    let mut input = "123.45";

    let result =  parse_regular_decimal_real(&mut input);
    assert_eq!(result.unwrap(), 123.45);
}

#[test]
fn test_parse_namespace() {
    let mut input = "namespacesome.";
    
    let result = parse_namespace(&mut input);

    assert!(matches!(result, Ok(Some(SymbolicName::UnescapedSymbolicName(v))) if v == "namespacesome"));
}

#[test]
fn test_parse_decimal_integer() {
    let mut input = "1234";
    let result = parse_decimal_integer(&mut input);
    
    assert!(matches!(result, Ok(num) if num == 1234));
}

#[test]
fn test_parse_octal_integer() {
    let mut input = "0o12344";
    let result = parse_octal_integer(&mut input);
    
    assert!(matches!(result, Ok(num) if num == 5348));
}

#[test]
fn test_parse_hex_integer() {
    let mut input = "0xFF12";
    let result = parse_hex_integer(&mut input);
    
    assert!(matches!(result, Ok(num) if num == 65298));
}

#[test]
fn test_parse_whitespace() {
    let mut input = r#"
            /* test comment */
    "#;

    let result = parse_whitespace(&mut input);
    assert!(result.is_ok());
}

#[test]
fn test_line_comment() {
    let mut input = "//some text";

    let result = parse_comment(&mut input);
    assert!(result.is_ok());
}

#[test]
fn test_multiline_comment() {
    let mut input = "/* some text */";

    let result = parse_comment(&mut input);
    assert!(result.is_ok());
}

#[test]
fn test_parse_symbolic_name_ident() {
    let mut input = "identifier";

    let result = parse_symbolic_name(&mut input);
    assert!(matches!(result, Ok(SymbolicName::UnescapedSymbolicName(s)) if s == "identifier"));
}

#[test]
fn test_parse_symbolic_name_escaped() {
    let mut input = "`identifier`";

    let result = parse_symbolic_name(&mut input);
    assert!(matches!(result, Ok(SymbolicName::EscapedSymbolicName(s)) if s == "identifier"));
}